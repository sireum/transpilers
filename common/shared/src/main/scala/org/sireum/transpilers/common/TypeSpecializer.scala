// #Sireum
/*
 Copyright (c) 2017-2021, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.transpilers.common

import org.sireum._
import org.sireum.alir.CallGraph
import org.sireum.message._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver._
import org.sireum.lang.tipe.TypeChecker.TypeRelation
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}

object TypeSpecializer {

  @datatype trait EntryPoint

  object EntryPoint {

    @datatype class App(val name: QName) extends EntryPoint

    @datatype class Worksheet(val program: AST.TopUnit.Program) extends EntryPoint

  }

  @datatype class NamedType(
    val tpe: AST.Typed.Name,
    @hidden val constructorVars: Map[String, (B, B, AST.Typed)],
    @hidden val vars: Map[String, (B, AST.Typed, AST.AssignExp)]
  )

  @datatype class Result(
    val typeHierarchy: TypeHierarchy,
    val entryPoints: ISZ[EntryPoint],
    val nameTypes: HashSMap[QName, HashSSet[NamedType]],
    val otherTypes: HashSSet[AST.Typed],
    val objectVars: HashSMap[QName, HashSSet[String]],
    val traitMethods: HashSSet[SMethod],
    val methods: HashSMap[QName, HashSSet[Method]],
    val extMethods: HashSSet[SMethod],
    val forwarding: HashMap[QName, QName],
    val typeImpl: Poset[AST.Typed.Name],
    val callGraph: Graph[SMember, B]
  )

  @datatype class Method(val receiverOpt: Option[AST.Typed.Name], val info: Info.Method)

  @datatype trait SMember {
    def receiverOpt: Option[AST.Typed.Name]
    def owner: QName
    def id: String
    def tpe: AST.Typed
  }

  @datatype class SVar(val receiverOpt: Option[AST.Typed.Name], val owner: QName, val id: String, val tpe: AST.Typed)
      extends SMember {
    override def string: String = {
      val sep: String = if (receiverOpt.nonEmpty) "#" else "."
      return st"${(AST.Typed.short(owner), ".")}$sep$id: $tpe".render
    }
  }

  @datatype class SMethod(
    val receiverOpt: Option[AST.Typed.Name],
    val owner: QName,
    val id: String,
    val tpe: AST.Typed.Fun,
    val mode: AST.MethodMode.Type
  ) extends SMember {
    override def string: String = {
      val sep: String = if (receiverOpt.nonEmpty) "#" else "."
      return st"[$mode] ${(AST.Typed.short(owner), ".")}$sep$id: $tpe".render
    }
  }

  val tsKind: String = "Type Specializer"
  val emptyCVars: Map[String, (B, B, AST.Typed)] = Map.empty
  val emptyVars: Map[String, (B, AST.Typed, AST.AssignExp)] = Map.empty
  val objectConstructorType: AST.Typed.Fun = AST.Typed.Fun(F, F, ISZ(), AST.Typed.unit)
  val atExitType: AST.Typed.Fun = AST.Typed.Fun(F, F, ISZ(), AST.Typed.unit)

  val mainTpe: AST.Typed.Fun =
    AST.Typed.Fun(F, F, ISZ(AST.Typed.Name(AST.Typed.isName, ISZ(AST.Typed.z, AST.Typed.string))), AST.Typed.z)

  val preResultExp: AST.MTransformer.PreResult[AST.Exp] = AST.MTransformer.PreResultExpInvoke(continu = F)

  def specialize(
    th: TypeHierarchy,
    entryPoints: ISZ[EntryPoint],
    forwarding: HashMap[QName, QName],
    reporter: Reporter
  ): TypeSpecializer.Result = {
    val ts = TypeSpecializer(th, entryPoints, forwarding)
    val r = ts.specialize()
    reporter.reports(ts.reporter.messages)
    return r
  }

}

import TypeSpecializer._

@record class TypeSpecializer(val th: TypeHierarchy, val eps: ISZ[EntryPoint], val forwarding: HashMap[QName, QName])
    extends AST.MTransformer {
  val reporter: Reporter = Reporter.create
  val methodRefinement: Poset[CallGraph.Node] = CallGraph.methodRefinements(th)
  var nameTypes: HashSMap[QName, HashSSet[NamedType]] = HashSMap.empty
  var otherTypes: HashSSet[AST.Typed] = HashSSet.empty
  var objectVars: HashSMap[QName, HashSSet[String]] = HashSMap.empty
  var methods: HashSMap[QName, HashSSet[Method]] = HashSMap.empty
  var callGraph: Graph[SMember, B] = Graph.empty
  var traitMethods: HashSSet[SMethod] = HashSSet.empty
  var extMethods: HashSSet[SMethod] = HashSSet.empty
  var workList: ISZ[Method] = ISZ()
  var seen: HashSSet[SMethod] = HashSSet.empty
  var descendantsCache: HashSMap[Poset.Index, HashSSet[Poset.Index]] = HashSMap.empty
  var currReceiverOpt: Option[AST.Typed.Name] = None()
  var currSMethodOpt: Option[SMethod] = None()

  def specialize(): TypeSpecializer.Result = {

    def entryApp(ep: EntryPoint.App): Unit = {
      th.nameMap.get(ep.name) match {
        case Some(inf: Info.Object) if inf.ast.isApp =>
          val name = inf.name :+ "main"
          th.nameMap.get(name) match {
            case Some(m: Info.Method) =>
              m.typedOpt.get.asInstanceOf[AST.Typed.Method].tpe match {
                case `mainTpe` =>
                  addType(mainTpe.args(0))
                  workList = workList :+ Method(None(), m)
                  th.nameMap.get(inf.name :+ "atExit") match {
                    case Some(atexit: Info.Method) =>
                      if (atexit.methodType.tpe == atExitType) {
                        workList = workList :+ Method(None(), atexit)
                      } else {
                        reporter.error(
                          None(),
                          tsKind,
                          st"'${(ep.name, ".")}' app's atExit method is not of type $atExitType.".render
                        )
                      }
                    case _ =>
                  }
                case _ =>
                  reporter
                    .error(None(), tsKind, st"'${(ep.name, ".")}' app's main method is not of type $mainTpe.".render)
              }
            case _ => reporter.error(None(), tsKind, st"'${(ep.name, ".")}' app does not have a main method.".render)
          }
        case Some(_) => reporter.error(None(), tsKind, st"'${(ep.name, ".")}' is not an app.".render)
        case _ => reporter.error(None(), tsKind, st"Could not find app '${(ep.name, ".")}'.".render)
      }
    }

    def entryWorksheet(ep: EntryPoint.Worksheet): Unit = {
      for (stmt <- ep.program.body.stmts) {
        val shouldTransform: B = stmt match {
          case _: AST.Stmt.Match => T
          case _: AST.Stmt.While => T
          case _: AST.Stmt.For => T
          case _: AST.Stmt.If => T
          case _: AST.Stmt.Block => T
          case _: AST.Stmt.DoWhile => T
          case _: AST.Stmt.Assign => T
          case _: AST.Stmt.Expr => T
          case _: AST.Stmt.Var => T
          case _: AST.Stmt.VarPattern => T
          case _: AST.Stmt.Spec => F
          case _: AST.Stmt.TypeAlias => F
          case _: AST.Stmt.SpecMethod => F
          case _: AST.Stmt.Object => F
          case _: AST.Stmt.Enum => F
          case _: AST.Stmt.Sig => F
          case _: AST.Stmt.Adt => F
          case _: AST.Stmt.ExtMethod => F
          case _: AST.Stmt.JustMethod => F
          case _: AST.Stmt.Import => F
          case _: AST.Stmt.Method => F
          case _: AST.Stmt.Return => F
          case _: AST.Stmt.SpecVar => F
          case _: AST.Stmt.SubZ => F
        }
        if (shouldTransform) {
          transformStmt(stmt)
        }
      }
    }

    def addEntryPoints(): Unit = {
      for (ep <- eps) {
        ep match {
          case ep: EntryPoint.App => entryApp(ep)
          case ep: EntryPoint.Worksheet => entryWorksheet(ep)
        }
      }
    }

    def work(): Unit = {
      var oldSeenSize = z"0"
      var oldTraitMethodsSize = z"0"

      do {

        oldSeenSize = seen.size
        oldTraitMethodsSize = traitMethods.size

        val wl = workList
        workList = ISZ()

        for (m <- wl) {
          val set: HashSSet[Method] = methods.get(m.info.owner) match {
            case Some(s) => s
            case _ => HashSSet.empty
          }
          methods = methods + m.info.owner ~> (set + m)
          val mRes = m.info.methodRes
          val oldCurrReceiverOpt = currReceiverOpt
          val oldCurrSMethodOpt = currSMethodOpt
          currReceiverOpt = m.receiverOpt
          currSMethodOpt = Some(
            SMethod(currReceiverOpt, m.info.owner, m.info.ast.sig.id.value, mRes.tpeOpt.get, mRes.mode)
          )
          for (stmt <- m.info.ast.bodyOpt.get.stmts) {
            transformStmt(stmt)
          }
          currReceiverOpt = oldCurrReceiverOpt
          currSMethodOpt = oldCurrSMethodOpt
        }

        for (tm <- traitMethods.elements) {
          val p = Poset.Internal.descendantsCache(th.poset, th.poset.nodes.get(tm.owner).get, descendantsCache)
          descendantsCache = p._2
          val ni = th.poset.nodesInverse
          val id = tm.id
          val t = tm.receiverOpt.get
          for (i <- p._1.elements) {
            val name = ni(i)
            th.typeMap.get(name).get match {
              case info: TypeInfo.Adt if !info.ast.isRoot =>
                val posOpt = info.ast.posOpt
                nameTypes.get(name) match {
                  case Some(nts) =>
                    for (nt <- nts.elements if th.isSubType(nt.tpe, t)) {
                      val receiver = nt.tpe
                      val aSubstMapOpt =
                        TypeChecker.buildTypeSubstMap(info.name, posOpt, info.ast.typeParams, receiver.args, reporter)
                      info.methods.get(id) match {
                        case Some(m) =>
                          val mFun = m.methodType.tpe.subst(aSubstMapOpt.get)
                          val smOpt =
                            TypeChecker.unifyFun(tsKind, th, posOpt, TypeRelation.Subtype, tm.tpe, mFun, reporter)
                          smOpt match {
                            case Some(sm) =>
                              val target = SMethod(Some(receiver), receiver.ids, id, mFun.subst(sm), tm.mode)
                              callGraph = callGraph + tm ~> target
                              addSMethod(posOpt, target)
                            case _ =>
                          }
                        case _ =>
                          val v = info.vars.get(id).get
                          val target = SVar(Some(receiver), receiver.ids, id, v.typedOpt.get.subst(aSubstMapOpt.get))
                          callGraph = callGraph + tm ~> target
                      }
                    }
                  case _ =>
                }
              case _ =>
            }
          }
        }

      } while (workList.nonEmpty)
    }

    addEntryPoints()

    work()

    var typeImpl = Poset.empty[AST.Typed.Name]

    def buildLeaves(info: TypeInfo): Unit = {
      val traits: ISZ[NamedType] = info match {
        case info: TypeInfo.Adt if info.ast.isRoot =>
          nameTypes.get(info.name) match {
            case Some(nts) => nts.elements
            case _ => return
          }
        case info: TypeInfo.Sig =>
          nameTypes.get(info.name) match {
            case Some(nts) => nts.elements
            case _ => return
          }
        case _ => return
      }
      for (nt <- traits) {
        val tpe = nt.tpe
        th.poset.nodes.get(tpe.ids) match {
          case Some(i) =>
            val p = Poset.Internal.descendantsCache(th.poset, i, descendantsCache)
            descendantsCache = p._2
            var subClasses = ISZ[AST.Typed.Name]()
            for (j <- p._1.elements) {
              nameTypes.get(th.poset.nodesInverse(j)) match {
                case Some(subs) =>
                  for (sub <- subs.elements) {
                    val subTpe = sub.tpe
                    th.typeMap.get(subTpe.ids).get match {
                      case subInfo: TypeInfo.Adt if !subInfo.ast.isRoot =>
                        if (th.isSubType(subTpe, tpe)) {
                          subClasses = subClasses :+ subTpe
                        }
                      case _ =>
                    }
                  }
                case _ =>
              }
            }
            typeImpl = typeImpl.addChildren(tpe, subClasses)
          case _ =>
        }
      }
    }

    for (info <- th.typeMap.values) {
      buildLeaves(info)
    }

    return TypeSpecializer.Result(
      th,
      eps,
      nameTypes,
      otherTypes,
      objectVars,
      traitMethods,
      methods,
      extMethods,
      forwarding,
      typeImpl,
      callGraph
    )
  }

  def classMethodImpl(posOpt: Option[Position], method: SMethod): Info.Method = {

    def combineSm(sm1: HashMap[String, AST.Typed], sm2: HashMap[String, AST.Typed]): HashMap[String, AST.Typed] = {
      if (sm1.isEmpty) {
        return sm2
      }
      var r = sm2
      for (e <- sm1.entries) {
        val (k, v) = e
        sm2.get(k) match {
          case Some(v2) => assert(v == v2)
          case _ => r = r + k ~> v
        }
      }
      return r
    }

    val receiver = method.receiverOpt.get
    val info = th.typeMap.get(receiver.ids).get.asInstanceOf[TypeInfo.Adt]
    assert(!info.ast.isRoot)
    val m: Info.Method = {
      val cm = info.methods.get(method.id).get
      if (cm.ast.bodyOpt.nonEmpty) {
        cm
      } else {
        def subst(m: Info.Method, name: QName, typeParams: ISZ[AST.TypeParam]): Info.Method = {
          for (ancestor <- info.ancestors if ancestor.ids == name) {
            val sm = TypeChecker.buildTypeSubstMap(info.name, posOpt, typeParams, ancestor.args, reporter).get
            return Info.substMethod(m, sm)
          }
          halt("Infeasible")
        }
        var owners = ISZ(cm.owner)
        var found: Option[Info.Method] = None()
        do {
          val curr = owners
          owners = ISZ()
          for (owner <- curr if found.isEmpty) {
            th.typeMap.get(owner).get match {
              case tInfo: TypeInfo.Adt =>
                val m = tInfo.methods.get(method.id).get
                if (m.ast.bodyOpt.isEmpty) {
                  for (p <- tInfo.ast.parents) {
                    owners = owners :+ p.typedOpt.get.asInstanceOf[AST.Typed.Name].ids
                  }
                } else {
                  val r = subst(m, tInfo.name, tInfo.ast.typeParams)
                  found = Some(r)
                }
              case tInfo: TypeInfo.Sig =>
                val m = tInfo.methods.get(method.id).get
                if (m.ast.bodyOpt.isEmpty) {
                  for (p <- tInfo.ast.parents) {
                    owners = owners :+ p.typedOpt.get.asInstanceOf[AST.Typed.Name].ids
                  }
                } else {
                  val r = subst(m, tInfo.name, tInfo.ast.typeParams)
                  found = Some(r)
                }
              case _ => halt("Infeasible")
            }
          }
        } while (owners.nonEmpty && found.isEmpty)
        assert(found.nonEmpty, "Infeasible")
        found.get
      }
    }

    val aSubstMapOpt = TypeChecker.buildTypeSubstMap(info.name, posOpt, info.ast.typeParams, receiver.args, reporter)
    val mFun = m.methodRes.tpeOpt.get
    val mSubstMapOpt = TypeChecker.unify(th, posOpt, TypeRelation.Equal, method.tpe, mFun, reporter)
    val substMap = combineSm(aSubstMapOpt.get, mSubstMapOpt.get)
    return Info.substMethod(m, substMap)
  }

  def addClassSVar(
    posOpt: Option[Position],
    caller: SMethod,
    receiver: AST.Typed.Name,
    v: AST.ResolvedInfo.Var
  ): Unit = {
    val info = th.typeMap.get(receiver.ids).get.asInstanceOf[TypeInfo.Adt]
    val aSubstMapOpt = TypeChecker.buildTypeSubstMap(receiver.ids, posOpt, info.ast.typeParams, receiver.args, reporter)
    val vInfo = info.vars.get(v.id).get
    val target = SVar(Some(receiver), v.owner, v.id, vInfo.ast.tipeOpt.get.typedOpt.get.subst(aSubstMapOpt.get))
    callGraph = callGraph + caller ~> target
  }

  def addSMethod(posOpt: Option[Position], m: SMethod): Unit = {
    m.mode match {
      case AST.MethodMode.Method =>
      case AST.MethodMode.Ext =>
      case _ => return
    }

    val method: SMethod = if (m.receiverOpt.isEmpty) {
      forwarding.get(m.owner) match {
        case Some(owner) => val r = m(owner = owner); callGraph = callGraph + m ~> r; r
        case _ => m
      }
    } else {
      m
    }

    if (seen.contains(method)) {
      return
    }
    seen = seen + method

    method.receiverOpt match {
      case Some(receiver) =>
        th.typeMap.get(receiver.ids).get match {
          case info: TypeInfo.Adt =>
            if (info.ast.isRoot) {
              traitMethods = traitMethods + method
            } else if (info.methods.contains(method.id)) {
              val mInfo = classMethodImpl(posOpt, method)
              workList = workList :+ Method(method.receiverOpt, mInfo)
            }
          case info: TypeInfo.Sig =>
            if (!info.ast.isExt) {
              traitMethods = traitMethods + method
            }
          case _ => halt("Infeasible")
        }
      case _ =>
        th.nameMap.get(method.owner :+ method.id).get match {
          case info: Info.Method =>
            if (info.ast.sig.typeParams.isEmpty) {
              workList = workList :+ Method(method.receiverOpt, info)
              return
            }
            val mType = info.methodType.tpe
            val substMapOpt = TypeChecker.unifyFun(tsKind, th, posOpt, TypeRelation.Equal, method.tpe, mType, reporter)
            substMapOpt match {
              case Some(substMap) => workList = workList :+ Method(method.receiverOpt, Info.substMethod(info, substMap))
              case _ =>
            }
          case _: Info.ExtMethod => extMethods = extMethods + method
          case _ => halt("Infeasible")
        }
    }
  }

  def addResolvedMethod(
    posOpt: Option[Position],
    m: AST.ResolvedInfo.Method,
    receiverOpt: Option[AST.Typed.Name],
    expType: AST.Typed
  ): Unit = {
    val rOpt: Option[AST.Typed.Name] = if (m.isInObject || m.owner.isEmpty) {
      None()
    } else if (th.typeMap.get(m.owner).nonEmpty) {
      m.mode match {
        case AST.MethodMode.Method =>
          receiverOpt match {
            case Some(_) => receiverOpt
            case _ => Some(currReceiverOpt.get)
          }
        case AST.MethodMode.Spec =>
          receiverOpt match {
            case Some(_) => receiverOpt
            case _ => Some(currReceiverOpt.get)
          }
        case AST.MethodMode.ObjectConstructor => None()
        case AST.MethodMode.Constructor => Some(expType.asInstanceOf[AST.Typed.Name])
        case AST.MethodMode.Copy => Some(expType.asInstanceOf[AST.Typed.Name])
        case AST.MethodMode.Extractor => None()
        case AST.MethodMode.Ext => None()
        case AST.MethodMode.Just => None()
        case AST.MethodMode.Select =>
          val mFun = m.tpeOpt.get
          Some(
            AST.Typed.Name(if (m.id == string"IS") AST.Typed.isName else AST.Typed.msName, ISZ(mFun.args(0), mFun.ret))
          )
        case AST.MethodMode.Store => Some(expType.asInstanceOf[AST.Typed.Name])
      }
    } else {
      // nested method, skip
      return
    }
    val target = SMethod(rOpt, m.owner, m.id, m.tpeOpt.get, m.mode)
    currSMethodOpt match {
      case Some(sm) => callGraph = callGraph + sm ~> target
      case _ =>
    }
    addSMethod(posOpt, target)
  }

  def specializeClass(t: AST.Typed.Name, info: TypeInfo.Adt): NamedType = {
    val oldRcvOpt = currReceiverOpt
    val oldSMethodOpt = currSMethodOpt
    val constructorInfo = info.constructorResOpt.get.asInstanceOf[AST.ResolvedInfo.Method]
    val smOpt = TypeChecker.buildTypeSubstMap(t.ids, info.ast.posOpt, info.ast.typeParams, t.args, reporter)
    val sm = smOpt.get
    currReceiverOpt = Some(t)
    currSMethodOpt = Some(
      SMethod(
        currReceiverOpt,
        constructorInfo.owner,
        constructorInfo.id,
        constructorInfo.tpeOpt.get.subst(sm),
        constructorInfo.mode
      )
    )
    var cvs = emptyCVars
    var vs = emptyVars
    for (p <- info.ast.params) {
      cvs = cvs + p.id.value ~> ((p.isVal, p.isHidden, p.tipe.typedOpt.get.subst(sm)))
    }
    for (stmt <- info.ast.stmts) {
      stmt match {
        case stmt: AST.Stmt.Var =>
          val vt = stmt.tipeOpt.get.typedOpt.get.subst(sm)
          addType(vt)
          val ae = AST.Util.substAssignExp(stmt.initOpt.get, sm)
          vs = vs + stmt.id.value ~> ((stmt.isVal, vt, ae))
          transformAssignExp(ae)
        case _ =>
      }
    }
    currReceiverOpt = oldRcvOpt
    currSMethodOpt = oldSMethodOpt
    return NamedType(t, cvs, vs)
  }

  def addType(o: AST.Typed): Unit = {
    if (o.hasTypeVars) {
      return
    }
    o match {
      case o: AST.Typed.Name =>
        val set: HashSSet[NamedType] = nameTypes.get(o.ids) match {
          case Some(s) => s
          case _ => HashSSet.empty
        }
        val key = NamedType(o, emptyCVars, emptyVars)
        val newSet: HashSSet[NamedType] = th.typeMap.get(o.ids).get match {
          case info: TypeInfo.Adt if !info.ast.isRoot =>
            if (set.contains(NamedType(o, emptyCVars, emptyVars))) {
              set
            } else {
              val nt = specializeClass(o, info)
              set + nt
            }
          case _ => set + key
        }
        if (set.size != newSet.size) {
          nameTypes = nameTypes + o.ids ~> newSet
          o.ids match {
            case AST.Typed.optionName =>
              o.args(0) match {
                case AST.Typed.b =>
                  addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.b)))
                  addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.b)))
                case AST.Typed.c =>
                  addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.c)))
                  addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.c)))
                case AST.Typed.z =>
                  addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.z)))
                  addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.z)))
                case AST.Typed.f32 =>
                  addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.f32)))
                  addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.f32)))
                case AST.Typed.f64 =>
                  addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.f64)))
                  addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.f64)))
                case AST.Typed.r =>
                  addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.r)))
                  addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.r)))
                case _ =>
                  th.typeMap.get(o.ids) match {
                    case Some(_: TypeInfo.SubZ) =>
                      addType(AST.Typed.Name(AST.Typed.someName, ISZ(AST.Typed.Name(o.ids, ISZ()))))
                      addType(AST.Typed.Name(AST.Typed.noneName, ISZ(AST.Typed.Name(o.ids, ISZ()))))
                    case _ =>
                  }
              }
            case _ =>
          }
        }
      case _: AST.Typed.Enum => // skip
      case _: AST.Typed.Tuple => otherTypes = otherTypes + o
      case _: AST.Typed.Fun => // skip
      case _: AST.Typed.Object => // skip
      case _: AST.Typed.Method => // skip
      case _: AST.Typed.Methods => halt("Infeasible")
      case _: AST.Typed.Package => // skip
      case _: AST.Typed.TypeVar => halt("Infeasible")
      case _: AST.Typed.Fact => halt("Infeasible")
      case _: AST.Typed.Theorem => halt("Infeasible")
      case _: AST.Typed.Inv => halt("Infeasible")
    }
  }

  def specializeObjectVar(info: Info.Var): Unit = {
    th.nameMap.get(info.owner).get match {
      case ownerInfo: Info.Object =>
        val constructorInfo = ownerInfo.constructorRes
        val oldRcvOpt = currReceiverOpt
        val oldSMethodOpt = currSMethodOpt
        currReceiverOpt = None()
        currSMethodOpt = {
          Some(
            SMethod(
              None(),
              constructorInfo.owner,
              constructorInfo.id,
              objectConstructorType,
              AST.MethodMode.ObjectConstructor
            )
          )
        }
        addType(info.ast.tipeOpt.get.typedOpt.get)
        transformAssignExp(info.ast.initOpt.get)
        currReceiverOpt = oldRcvOpt
        currSMethodOpt = oldSMethodOpt
      case _: Info.Package =>
        assert(info.owner == AST.Typed.sireumName && (info.ast.id.value == string"T" || info.ast.id.value == string"F"))
      case _ => halt("Infeasible")
    }

  }

  override def preResolvedAttr(o: AST.ResolvedAttr): AST.MTransformer.PreResult[AST.ResolvedAttr] = {
    def addSomeNone(t: AST.Typed): Unit = {
      addType(AST.Typed.Name(AST.Typed.someName, ISZ(t)))
      addType(AST.Typed.Name(AST.Typed.noneName, ISZ(t)))
    }
    o.resOpt.get match {
      case res: AST.ResolvedInfo.Var =>
        if (res.isInObject && !res.isSpec && res.owner.size != z"0" && res.owner != AST.Typed.sireumName) {
          val info = th.nameMap.get(res.owner :+ res.id).get.asInstanceOf[Info.Var]
          val set: HashSSet[String] = objectVars.get(res.owner) match {
            case Some(s) => s
            case _ => HashSSet.empty
          }
          val newSet = set + res.id
          if (newSet.size != set.size) {
            objectVars = objectVars + res.owner ~> newSet
            specializeObjectVar(info)
          }
          val target = SVar(None(), res.owner, res.id, info.ast.tipeOpt.get.typedOpt.get)
          currSMethodOpt match {
            case Some(sm) => callGraph = callGraph + sm ~> target
            case _ =>
          }
        }
      case _: AST.ResolvedInfo.LocalVar => // skip
      case res: AST.ResolvedInfo.Method =>
        if (res.mode == AST.MethodMode.Constructor && th.typeMap
            .get(res.owner :+ res.id)
            .get
            .isInstanceOf[TypeInfo.SubZ]) {
          addSomeNone(res.tpeOpt.get.ret.asInstanceOf[AST.Typed.Name].args(0))
        }
      case res: AST.ResolvedInfo.BuiltIn =>
        def addEnumOpt(): Unit = {
          o.typedOpt.get match {
            case AST.Typed.Name(AST.Typed.optionName, args) => addSomeNone(args(0))
            case _ =>
          }
        }
        res.kind match {
          case AST.ResolvedInfo.BuiltIn.Kind.EnumByName => addEnumOpt()
          case AST.ResolvedInfo.BuiltIn.Kind.EnumByOrdinal => addEnumOpt()
          case _ => // skip
        }
      case _: AST.ResolvedInfo.Tuple => // skip
      case _: AST.ResolvedInfo.Enum => // skip
      case _: AST.ResolvedInfo.EnumElement => // skip
      case _: AST.ResolvedInfo.Object => // skip
      case _: AST.ResolvedInfo.Package => // skip
      case _: AST.ResolvedInfo.Methods => halt("Infeasible")
      case _: AST.ResolvedInfo.Fact => halt("Infeasible")
      case _: AST.ResolvedInfo.Theorem => halt("Infeasible")
      case _: AST.ResolvedInfo.Inv => halt("Infeasible")
    }
    return AST.MTransformer.PreResultResolvedAttr
  }

  override def preExpIdent(o: AST.Exp.Ident): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case v: AST.ResolvedInfo.Var if !v.isInObject =>
        currSMethodOpt match {
          case Some(sm) => addClassSVar(o.posOpt, sm, currReceiverOpt.get, v)
          case _ =>
        }
      case m: AST.ResolvedInfo.Method =>
        if (m.isInObject) {
          addResolvedMethod(o.posOpt, m, None(), m.tpeOpt.get.ret)
        } else {
          if (currReceiverOpt.nonEmpty) {
            addResolvedMethod(o.posOpt, m, currReceiverOpt, m.tpeOpt.get.ret)
          }
        }
      case _ =>
    }
    return AST.MTransformer.PreResultExpIdent
  }

  override def preExpEta(o: AST.Exp.Eta): AST.MTransformer.PreResult[AST.Exp] = {
    o.ref match {
      case ref: AST.Exp.Ident =>
        ref.attr.resOpt.get match {
          case m: AST.ResolvedInfo.Method =>
            val rOpt: Option[AST.Typed.Name] = if (m.isInObject) {
              None()
            } else if (th.typeMap.get(m.owner).nonEmpty) {
              assert(currReceiverOpt.nonEmpty)
              currReceiverOpt
            } else {
              // nested method, skip
              return AST.MTransformer.PreResultExpEta
            }
            addSMethod(ref.posOpt, SMethod(rOpt, m.owner, m.id, m.tpeOpt.get, m.mode))
          case _ =>
        }
      case _ =>
    }
    otherTypes = otherTypes + o.typedOpt.get
    return AST.MTransformer.PreResultExpEta
  }

  override def preExpFun(o: AST.Exp.Fun): AST.MTransformer.PreResult[AST.Exp] = {
    otherTypes = otherTypes + o.typedOpt.get
    return AST.MTransformer.PreResultExpFun
  }

  override def preExpSelect(o: AST.Exp.Select): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, receiverTypeOpt(o.receiverOpt), o.typedOpt.get)
      case v: AST.ResolvedInfo.Var if !v.isInObject =>
        currSMethodOpt match {
          case Some(sm) =>
            val receiver: AST.Typed.Name = o.receiverOpt match {
              case Some(r) =>
                r.typedOpt.get match {
                  case t: AST.Typed.Method if t.tpe.isByName => t.tpe.ret.asInstanceOf[AST.Typed.Name]
                  case t: AST.Typed.Name => t
                  case _ => halt("Infeasible")
                }
              case _ => currReceiverOpt.get
            }
            addClassSVar(o.posOpt, sm, receiver, v)
          case _ =>
        }
      case _ =>
    }
    return AST.MTransformer.PreResultExpSelect
  }

  override def preExpInvoke(o: AST.Exp.Invoke): AST.MTransformer.PreResult[AST.Exp] = {
    if (o.ident.id.value != string"apply") {
      o.attr.resOpt.get match {
        case m: AST.ResolvedInfo.Method =>
          addResolvedMethod(o.posOpt, m, receiverTypeOpt(o.receiverOpt), o.typedOpt.get)
        case AST.ResolvedInfo.BuiltIn(kind) if kind == AST.ResolvedInfo.BuiltIn.Kind.Print || kind == AST.ResolvedInfo.BuiltIn.Kind.Println =>
          for (arg <- o.args) {
            arg.typedOpt.get match {
              case t: AST.Typed.Name =>
                th.typeMap.get(t.ids).get match {
                  case ti: TypeInfo.Adt if ti.methods.contains("string") =>
                    val m = AST.ResolvedInfo.Method(F, AST.MethodMode.Method, ISZ(), t.ids, "string", ISZ(),
                      Some(AST.Typed.Fun(F, T, ISZ(), AST.Typed.string)), ISZ(), ISZ())
                    addResolvedMethod(o.posOpt, m, Some(t), o.typedOpt.get)
                  case _ =>
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
    AST.MTransformer.transformOption(o.receiverOpt, transformExp _)
    transformResolvedAttr(o.ident.attr)
    AST.MTransformer.transformISZ(o.targs, transformType _)
    AST.MTransformer.transformISZ(o.args, transformExp _)
    transformResolvedAttr(o.attr)
    return preResultExp
  }

  override def preExpBinary(o: AST.Exp.Binary): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method =>
        addResolvedMethod(o.posOpt, m, Some(o.left.typedOpt.get.asInstanceOf[AST.Typed.Name]), o.typedOpt.get)
      case _ =>
    }
    transformExp(o.left)
    transformExp(o.right)
    transformResolvedAttr(o.attr)
    return preResultExp
  }

  override def preExpInvokeNamed(o: AST.Exp.InvokeNamed): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, receiverTypeOpt(o.receiverOpt), o.typedOpt.get)
      case _ =>
    }
    AST.MTransformer.transformOption(o.receiverOpt, transformExp _)
    transformResolvedAttr(o.ident.attr)
    AST.MTransformer.transformISZ(o.targs, transformType _)
    AST.MTransformer.transformISZ(o.args, transformNamedArg _)
    transformResolvedAttr(o.attr)
    return preResultExp
  }

  override def preTyped(o: AST.Typed): AST.MTransformer.PreResult[AST.Typed] = {
    addType(o)
    return AST.MTransformer.PreResultTypedName
  }

  @pure def receiverTypeOpt(eOpt: Option[AST.Exp]): Option[AST.Typed.Name] = {
    eOpt match {
      case Some(e) =>
        e.typedOpt.get match {
          case _: AST.Typed.Package => return None()
          case _: AST.Typed.Object => return None()
          case t: AST.Typed.Name => return Some(t)
          case t: AST.Typed.Method if t.tpe.isByName => Some(t.tpe.ret.asInstanceOf[AST.Typed.Name])
          case _ => halt(s"Infeasible: $e")
        }
      case _ => return None()
    }
  }

  override def transformStmt(o: AST.Stmt): MOption[AST.Stmt] = {
    o match {
      case _: AST.Stmt.Spec => return MNone()
      case _ => return super.transformStmt(o)
    }
  }

  override def transformMethodContract(o: AST.MethodContract): MOption[AST.MethodContract] = {
    return MNone()
  }

  override def transformLoopContract(o: AST.LoopContract): MOption[AST.LoopContract] = {
    return MNone()
  }
}
