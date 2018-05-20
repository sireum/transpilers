// #Sireum

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

    @datatype class Method(name: QName) extends EntryPoint

    @datatype class Worksheet(program: AST.TopUnit.Program) extends EntryPoint

  }

  @datatype class NamedType(
    tpe: AST.Typed.Name,
    @hidden constructorVars: Map[String, (B, AST.Typed)],
    @hidden vars: Map[String, (B, AST.Typed, AST.AssignExp)]
  )

  @datatype class Result(
    typeHierarchy: TypeHierarchy,
    entryPoints: ISZ[EntryPoint],
    nameTypes: HashSMap[QName, HashSSet[NamedType]],
    otherTypes: HashSSet[AST.Typed],
    objectVars: HashSMap[QName, HashSSet[String]],
    traitMethods: HashSSet[SMethod],
    methods: HashSMap[QName, HashSSet[Method]],
    typeImpl: Poset[AST.Typed.Name],
    callGraph: Graph[SMember, B]
  )

  @datatype class Method(receiverOpt: Option[AST.Typed.Name], info: Info.Method)

  @record class TypeSubstitutor(substMap: HashMap[String, AST.Typed]) extends AST.MTransformer {

    override def preTyped(o: AST.Typed): AST.MTransformer.PreResult[AST.Typed] = {
      o match {
        case o: AST.Typed.TypeVar =>
          substMap.get(o.id) match {
            case Some(t) => return AST.MTransformer.PreResult(F, MSome(t))
            case _ =>
          }
        case _ =>
      }
      val r = super.preTyped(o)
      return r
    }

  }

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
    mode: AST.MethodMode.Type
  ) extends SMember {
    override def string: String = {
      val sep: String = if (receiverOpt.nonEmpty) "#" else "."
      return st"[$mode] ${(AST.Typed.short(owner), ".")}$sep$id: $tpe".render
    }
  }

  val tsKind: String = "Type Specializer"
  val emptyCVars: Map[String, (B, AST.Typed)] = Map.empty
  val emptyVars: Map[String, (B, AST.Typed, AST.AssignExp)] = Map.empty
  val objectConstructorType: AST.Typed.Fun = AST.Typed.Fun(F, F, ISZ(), AST.Typed.unit)

  def specialize(th: TypeHierarchy, entryPoints: ISZ[EntryPoint], reporter: Reporter): Result = {
    val ts = TypeSpecializer(th, entryPoints)
    val r = ts.specialize()
    reporter.reports(ts.reporter.messages)
    return r
  }

  @pure def substMethod(m: Info.Method, substMap: HashMap[String, AST.Typed]): Info.Method = {
    if (substMap.nonEmpty) {
      val newAst = TypeSubstitutor(substMap).transformStmt(m.ast).get
      return m(ast = newAst.asInstanceOf[AST.Stmt.Method])
    } else {
      return m
    }
  }

  @pure def substAssignExp(ast: AST.AssignExp, substMap: HashMap[String, AST.Typed]): AST.AssignExp = {
    if (substMap.nonEmpty) {
      val newAst = TypeSubstitutor(substMap).transformAssignExp(ast).get
      return newAst.asInstanceOf[AST.AssignExp]
    } else {
      return ast
    }
  }

}

import TypeSpecializer._

@record class TypeSpecializer(th: TypeHierarchy, eps: ISZ[EntryPoint]) extends AST.MTransformer {
  val reporter: Reporter = Reporter.create
  val methodRefinement: Poset[CallGraph.Node] = CallGraph.methodRefinements(th)
  var nameTypes: HashSMap[QName, HashSSet[NamedType]] = HashSMap.empty
  var otherTypes: HashSSet[AST.Typed] = HashSSet.empty
  var objectVars: HashSMap[QName, HashSSet[String]] = HashSMap.empty
  var methods: HashSMap[QName, HashSSet[Method]] = HashSMap.empty
  var callGraph: Graph[SMember, B] = Graph.empty
  var traitMethods: HashSSet[SMethod] = HashSSet.empty
  var workList: ISZ[Method] = ISZ()
  var seen: HashSSet[SMethod] = HashSSet.empty
  var descendantsCache: HashMap[Poset.Index, HashSet[Poset.Index]] = HashMap.empty
  var currReceiverOpt: Option[AST.Typed.Name] = None()
  var currSMethodOpt: Option[SMethod] = None()

  def specialize(): TypeSpecializer.Result = {

    def entryMethod(ep: EntryPoint.Method): Unit = {
      val info: Info.Method = th.nameMap.get(ep.name) match {
        case Some(inf: Info.Method) => inf
        case Some(_) =>
          reporter.error(None(), tsKind, st"'${(ep.name, ".")}' is not a method.".render)
          return
        case _ =>
          reporter.error(None(), tsKind, st"Could not find method entry point '${(ep.name, ".")}'.".render)
          return
      }
      if (info.ast.sig.typeParams.nonEmpty) {
        reporter.error(None(), tsKind, st"Method entry point '${(ep.name, ".")}' cannot be generic.".render)
        return
      }

      workList = workList :+ Method(None(), info)
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
          case _: AST.Stmt.LStmt => F
          case _: AST.Stmt.TypeAlias => F
          case _: AST.Stmt.SpecMethod => F
          case _: AST.Stmt.Object => F
          case _: AST.Stmt.Enum => F
          case _: AST.Stmt.Sig => F
          case _: AST.Stmt.AbstractDatatype => F
          case _: AST.Stmt.ExtMethod => F
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
          case ep: EntryPoint.Method => entryMethod(ep)
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
              case info: TypeInfo.AbstractDatatype if !info.ast.isRoot =>
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
                            TypeChecker.unifyFun(tsKind, th, posOpt, TypeRelation.Supertype, tm.tpe, mFun, reporter)
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
        case info: TypeInfo.AbstractDatatype if info.ast.isRoot =>
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
                      case subInfo: TypeInfo.AbstractDatatype if !subInfo.ast.isRoot =>
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

    return Result(th, eps, nameTypes, otherTypes, objectVars, traitMethods, methods, typeImpl, callGraph)
  }

  def classMethodImpl(posOpt: Option[Position], method: SMethod): Info.Method = {

    def combine(sm1: HashMap[String, AST.Typed], sm2: HashMap[String, AST.Typed]): HashMap[String, AST.Typed] = {
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
    val info = th.typeMap.get(receiver.ids).get.asInstanceOf[TypeInfo.AbstractDatatype]
    assert(!info.ast.isRoot)
    val m: Info.Method = {
      val cm = info.methods.get(method.id).get
      if (cm.ast.bodyOpt.nonEmpty) {
        cm
      } else {
        var nodes: ISZ[CallGraph.Node] = ISZ(CallGraph.Node(T, F, cm.owner, cm.ast.sig.id.value))
        var mOpt: Option[Info.Method] = None()
        do {
          var parentNodes: ISZ[CallGraph.Node] = ISZ()
          for (node <- nodes if mOpt.isEmpty) {
            for (parent <- methodRefinement.parentsOf(node).elements if mOpt.isEmpty) {
              val cInfo: Info.Method = th.typeMap.get(parent.owner).get match {
                case tInfo: TypeInfo.AbstractDatatype => tInfo.methods.get(parent.id).get
                case tInfo: TypeInfo.Sig => tInfo.methods.get(parent.id).get
                case _ => halt("Infeasible")
              }
              if (cInfo.ast.bodyOpt.isEmpty) {
                parentNodes = parentNodes :+ CallGraph.Node(T, F, cInfo.owner, cInfo.ast.sig.id.value)
              } else {
                mOpt = Some(cInfo)
              }
            }
          }
          nodes = parentNodes
        } while (mOpt.isEmpty || nodes.isEmpty)
        mOpt.get(owner = method.owner)
      }
    }

    val aSubstMapOpt = TypeChecker.buildTypeSubstMap(info.name, posOpt, info.ast.typeParams, receiver.args, reporter)
    val mFun = m.methodRes.tpeOpt.get
    val mSubstMapOpt = TypeChecker.unify(th, posOpt, TypeRelation.Equal, method.tpe, mFun, reporter)
    val substMap = combine(aSubstMapOpt.get, mSubstMapOpt.get)
    return substMethod(m, substMap)
  }

  def addClassSVar(
    posOpt: Option[Position],
    caller: SMethod,
    receiver: AST.Typed.Name,
    v: AST.ResolvedInfo.Var
  ): Unit = {
    val info = th.typeMap.get(receiver.ids).get.asInstanceOf[TypeInfo.AbstractDatatype]
    val aSubstMapOpt = TypeChecker.buildTypeSubstMap(receiver.ids, posOpt, info.ast.typeParams, receiver.args, reporter)
    val vInfo = info.vars.get(v.id).get
    val target = SVar(Some(receiver), v.owner, v.id, vInfo.ast.tipeOpt.get.typedOpt.get.subst(aSubstMapOpt.get))
    callGraph = callGraph + caller ~> target
  }

  def addSMethod(posOpt: Option[Position], method: SMethod): Unit = {
    method.mode match {
      case AST.MethodMode.Method =>
      case _ => return
    }

    if (seen.contains(method)) {
      return
    }

    method.receiverOpt match {
      case Some(receiver) =>
        th.typeMap.get(receiver.ids).get match {
          case info: TypeInfo.AbstractDatatype if info.ast.isRoot => traitMethods = traitMethods + method
          case info: TypeInfo.Sig =>
            if (!info.ast.isExt) {
              traitMethods = traitMethods + method
            }
          case _: TypeInfo.AbstractDatatype =>
            val mInfo = classMethodImpl(posOpt, method)
            workList = workList :+ Method(method.receiverOpt, mInfo)
          case _ => halt("Infeasible")
        }
      case _ =>
        val m = th.nameMap.get(method.owner :+ method.id).get.asInstanceOf[Info.Method]
        seen = seen + method
        if (m.ast.sig.typeParams.isEmpty) {
          workList = workList :+ Method(method.receiverOpt, m)
          return
        }
        val mType = m.methodType.tpe
        val substMapOpt = TypeChecker.unifyFun(tsKind, th, posOpt, TypeRelation.Equal, method.tpe, mType, reporter)
        substMapOpt match {
          case Some(substMap) => workList = workList :+ Method(method.receiverOpt, substMethod(m, substMap))
          case _ =>
        }
    }
  }

  def addResolvedMethod(
    posOpt: Option[Position],
    m: AST.ResolvedInfo.Method,
    receiverOpt: Option[AST.Typed.Name],
    expType: AST.Typed
  ): Unit = {
    val rOpt: Option[AST.Typed.Name] = if (m.isInObject) {
      None()
    } else if (th.typeMap.get(m.owner).nonEmpty) {
      m.mode match {
        case AST.MethodMode.Method =>
          receiverOpt match {
            case Some(_) => receiverOpt
            case _ =>
              if (currReceiverOpt.isEmpty) {
                println("here")
              }
              Some(currReceiverOpt.get)
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

  def specializeClass(t: AST.Typed.Name, info: TypeInfo.AbstractDatatype): NamedType = {
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
      cvs = cvs + p.id.value ~> ((p.isVal, p.tipe.typedOpt.get.subst(sm)))
    }
    for (stmt <- info.ast.stmts) {
      stmt match {
        case stmt: AST.Stmt.Var =>
          val vt = stmt.tipeOpt.get.typedOpt.get.subst(sm)
          addType(vt)
          val ae = substAssignExp(stmt.initOpt.get, sm)
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
          case info: TypeInfo.AbstractDatatype if !info.ast.isRoot =>
            if (set.contains(NamedType(o, emptyCVars, emptyVars))) {
              set
            } else {
              val nt = specializeClass(o, info)
              set + nt
            }
          case _ => set + key
        }
        nameTypes = nameTypes + o.ids ~> newSet
      case _: AST.Typed.Enum => // skip
      case _: AST.Typed.Tuple => otherTypes = otherTypes + o
      case _: AST.Typed.Fun => // skip
      case _: AST.Typed.Object => // skip
      case _: AST.Typed.Method => // skip
      case _: AST.Typed.Methods => halt("Infeasible")
      case _: AST.Typed.Package => // skip
      case _: AST.Typed.TypeVar => halt("Infeasible")
    }
  }

  def specializeObjectVar(info: Info.Var): Unit = {
    th.nameMap.get(info.owner).get match {
      case ownerInfo: Info.Object =>
        val constructorInfo = ownerInfo.asInstanceOf[Info.Object].constructorRes
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
    o.resOpt.get match {
      case res: AST.ResolvedInfo.Var =>
        if (res.isInObject && !res.isSpec && res.owner.size != z"0") {
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
      case _: AST.ResolvedInfo.Method => // skip
      case _: AST.ResolvedInfo.BuiltIn => // skip
      case _: AST.ResolvedInfo.Tuple => // skip
      case _: AST.ResolvedInfo.Enum => // skip
      case _: AST.ResolvedInfo.EnumElement => // skip
      case _: AST.ResolvedInfo.Object => // skip
      case _: AST.ResolvedInfo.Package => // skip
      case _: AST.ResolvedInfo.Methods => halt("Infeasible")
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
              case Some(r) => r.typedOpt.get.asInstanceOf[AST.Typed.Name]
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
    if (o.ident.id.value == string"apply") {
      return AST.MTransformer.PreResultExpInvoke
    }
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, receiverTypeOpt(o.receiverOpt), o.typedOpt.get)
      case _ =>
    }

    return AST.MTransformer.PreResultExpInvoke
  }

  override def preExpInvokeNamed(o: AST.Exp.InvokeNamed): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, receiverTypeOpt(o.receiverOpt), o.typedOpt.get)
      case _ =>
    }
    return AST.MTransformer.PreResultExpInvokeNamed
  }

  override def preTyped(o: AST.Typed): AST.MTransformer.PreResult[AST.Typed] = {
    addType(o)
    return AST.MTransformer.PreResultTypedName
  }

  @pure def receiverTypeOpt(eOpt: Option[AST.Exp]): Option[AST.Typed.Name] = {
    eOpt match {
      case Some(e) =>
        e.typedOpt.get match {
          case _: AST.Typed.Object => return None()
          case _ => return Some(e.typedOpt.get.asInstanceOf[AST.Typed.Name])
        }
      case _ => return None()
    }
  }

}
