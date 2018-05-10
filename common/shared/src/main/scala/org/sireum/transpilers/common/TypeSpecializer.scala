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
    @hidden constructorVars: Map[String, AST.Typed],
    @hidden vars: Map[String, AST.Typed]
  )

  @datatype class Result(
    typeHierarchy: TypeHierarchy,
    entryPoints: ISZ[EntryPoint],
    nameTypes: HashMap[QName, HashSet[NamedType]],
    otherTypes: HashSet[AST.Typed],
    objectVars: HashSet[QName],
    methods: HashMap[QName, HashSet[Method]],
    funs: HashMap[QName, HashSet[AST.Exp.Fun]]
  )

  @datatype class Method(
    isInObject: B,
    isNested: B,
    owner: QName,
    id: String,
    ast: AST.Stmt.Method,
    closureEnv: HashMap[String, AST.Typed]
  )

  @record class TypeSubstitutor(substMap: HashMap[String, AST.Typed]) extends AST.MTransformer {

    override def preTyped(o: AST.Typed): AST.MTransformer.PreResult[AST.Typed] = {
      o match {
        case o: AST.Typed.TypeVar =>
          substMap.get(o.id) match {
            case Some(t) => return AST.MTransformer.PreResult(F, MSome(t))
            case _ => halt(s"Unexpected situation when substituting type var '${o.id}'.")
          }
        case _ =>
      }
      val r = super.preTyped(o)
      return r
    }

  }

  @datatype class SMethod(receiverOpt: Option[AST.Typed.Name], owner: QName, id: String, tpe: AST.Typed.Fun)

  val tsKind: String = "Type Specializer"
  val emptyVars: Map[String, AST.Typed] = Map.empty

  def specialize(th: TypeHierarchy, entryPoints: ISZ[EntryPoint], reporter: Reporter): Result = {
    val ts = TypeSpecializer(th, entryPoints)
    val r = ts.specialize()
    reporter.reports(ts.reporter.messages)
    return r
  }

  @pure def substMethod(m: Info.Method, substMap: HashMap[String, AST.Typed]): Info.Method = {
    if (substMap.nonEmpty) {
      val newAst = TypeSubstitutor(substMap).transformStmt(m.ast)
      return m(ast = newAst.asInstanceOf[AST.Stmt.Method])
    } else {
      return m
    }
  }

  @pure def substAssignExp(ast: AST.AssignExp, substMap: HashMap[String, AST.Typed]): AST.AssignExp = {
    if (substMap.nonEmpty) {
      val newAst = TypeSubstitutor(substMap).transformAssignExp(ast)
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
  var nameTypes: HashMap[QName, HashSet[NamedType]] = HashMap.empty
  var otherTypes: HashSet[AST.Typed] = HashSet.empty
  var objectVars: HashSet[QName] = HashSet.empty
  var methods: HashMap[QName, HashSet[Method]] = HashMap.empty
  var funs: HashMap[QName, HashSet[AST.Exp.Fun]] = HashMap.empty
  var traitMethods: ISZ[SMethod] = ISZ()
  var workList: ISZ[Info.Method] = ISZ()
  var seen: HashSet[SMethod] = HashSet.empty
  var decendantsCache: HashMap[Poset.Index, HashSet[Poset.Index]] = HashMap.empty
  var currReceiverOpt: Option[AST.Typed.Name] = None()

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

      workList = workList :+ info
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
          for (stmt <- m.ast.bodyOpt.get.stmts) {
            transformStmt(stmt)
          }
        }

        for (tm <- traitMethods) {
          val p = Poset.Internal.descendantsCache(th.poset, th.poset.nodes.get(tm.owner).get, decendantsCache)
          decendantsCache = p._2
          val ni = th.poset.nodesInverse
          val id = tm.id
          val t = tm.receiverOpt.get
          for (i <- p._1.elements) {
            val name = ni(i)
            th.typeMap.get(name).get match {
              case info: TypeInfo.AbstractDatatype if !info.ast.isRoot && info.methods.contains(id) =>
                val posOpt = info.ast.posOpt
                nameTypes.get(name) match {
                  case Some(nts) =>
                    for (nt <- nts.elements if th.isSubType(nt.tpe, t)) {
                      val aSubstMapOpt =
                        TypeChecker.buildTypeSubstMap(info.name, posOpt, info.ast.typeParams, nt.tpe.args, reporter)
                      val m = info.methods.get(id).get
                      val mFun = m.methodType.tpe.subst(aSubstMapOpt.get)
                      val smOpt =
                        TypeChecker.unifyFun(tsKind, th, posOpt, TypeRelation.Supertype, tm.tpe, mFun, reporter)
                      smOpt match {
                        case Some(sm) => addSMethod(posOpt, SMethod(Some(nt.tpe), nt.tpe.ids, id, mFun.subst(sm)))
                        case _ =>
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

    return TypeSpecializer.Result(th, eps, nameTypes, otherTypes, objectVars, methods, funs)
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

  def addSMethod(posOpt: Option[Position], method: SMethod): Unit = {
    if (seen.contains(method)) {
      return
    }

    method.receiverOpt match {
      case Some(receiver) =>
        th.typeMap.get(receiver.ids).get match {
          case info: TypeInfo.AbstractDatatype if info.ast.isRoot => traitMethods = traitMethods :+ method
          case _: TypeInfo.Sig => traitMethods = traitMethods :+ method
          case _: TypeInfo.AbstractDatatype =>
            val mInfo = classMethodImpl(posOpt, method)
            workList = workList :+ mInfo
          case _ => halt("Infeasible")
        }
      case _ =>
        val m = th.nameMap.get(method.owner :+ method.id).get.asInstanceOf[Info.Method]
        seen = seen + method
        if (m.ast.sig.typeParams.isEmpty) {
          workList = workList :+ m
          return
        }
        val mType = m.methodType.tpe
        val substMapOpt = TypeChecker.unifyFun(tsKind, th, posOpt, TypeRelation.Equal, method.tpe, mType, reporter)
        substMapOpt match {
          case Some(substMap) => workList = workList :+ substMethod(m, substMap)
          case _ =>
        }
    }
  }

  def addResolvedMethod(posOpt: Option[Position], m: AST.ResolvedInfo.Method, receiverOpt: Option[AST.Exp]): Unit = {
    val rOpt: Option[AST.Typed.Name] = if (m.isInObject) {
      None()
    } else if (th.typeMap.get(m.owner).nonEmpty) {
      receiverOpt match {
        case Some(receiver) => Some(receiver.typedOpt.get.asInstanceOf[AST.Typed.Name])
        case _ => Some(currReceiverOpt.get)
      }
    } else {
      // nested method, skip
      return
    }
    addSMethod(posOpt, SMethod(rOpt, m.owner, m.id, m.tpeOpt.get))
  }

  def specializeClass(t: AST.Typed.Name, info: TypeInfo.AbstractDatatype): NamedType = {
    val oldRcvOpt = currReceiverOpt
    currReceiverOpt = Some(t)
    val smOpt = TypeChecker.buildTypeSubstMap(t.ids, info.ast.posOpt, info.ast.typeParams, t.args, reporter)
    val sm = smOpt.get
    var cvs = emptyVars
    var vs = emptyVars
    for (p <- info.ast.params) {
      cvs = cvs + p.id.value ~> p.tipe.typedOpt.get.subst(sm)
    }
    for (stmt <- info.ast.stmts) {
      stmt match {
        case stmt: AST.Stmt.Var =>
          val vt = stmt.tipeOpt.get.typedOpt.get.subst(sm)
          vs = vs + stmt.id.value ~> vt
          addType(vt)
          transformAssignExp(substAssignExp(stmt.initOpt.get, sm))
        case _ =>
      }
    }
    currReceiverOpt = oldRcvOpt
    return NamedType(t, cvs, vs)
  }

  def addType(o: AST.Typed): Unit = {
    o match {
      case o: AST.Typed.Name =>
        val set: HashSet[NamedType] = nameTypes.get(o.ids) match {
          case Some(s) => s
          case _ => HashSet.empty
        }
        val key = NamedType(o, emptyVars, emptyVars)
        val newSet: HashSet[NamedType] = th.typeMap.get(o.ids).get match {
          case info: TypeInfo.AbstractDatatype if !info.ast.isRoot =>
            if (set.contains(NamedType(o, emptyVars, emptyVars))) {
              set
            } else {
              val nt = specializeClass(o, info)
              set + nt
            }
          case _ => set + key
        }
        nameTypes = nameTypes + o.ids ~> newSet
      case _: AST.Typed.Enum => otherTypes = otherTypes + o
      case _: AST.Typed.Tuple => otherTypes = otherTypes + o
      case _: AST.Typed.Object => // skip
      case _: AST.Typed.Fun => // skip
      case _: AST.Typed.Method => // skip
      case _: AST.Typed.Methods => // skip
      case _: AST.Typed.Package => // skip
      case _: AST.Typed.TypeVar => halt("Infeasible")
    }
  }

  def specializeObjectVar(info: Info.Var): Unit = {
    val oldRcvOpt = currReceiverOpt
    currReceiverOpt = None()
    addType(info.ast.tipeOpt.get.typedOpt.get)
    transformAssignExp(info.ast.initOpt.get)
    currReceiverOpt = oldRcvOpt
  }

  override def preResolvedAttr(o: AST.ResolvedAttr): AST.MTransformer.PreResult[AST.ResolvedAttr] = {
    o.resOpt.get match {
      case res: AST.ResolvedInfo.Var =>
        if (res.isInObject && !res.isSpec) {
          val info = th.nameMap.get(res.owner :+ res.id).get.asInstanceOf[Info.Var]
          val newObjectVars = objectVars + info.name
          if (newObjectVars.size != objectVars.size) {
            specializeObjectVar(info)
          }
          objectVars = newObjectVars
        }
      case res: AST.ResolvedInfo.LocalVar =>
        if (res.scope == AST.ResolvedInfo.LocalVar.Scope.Closure) {
          halt("TODO") // TODO: handle closure
        }
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
            addSMethod(ref.posOpt, SMethod(rOpt, m.owner, m.id, m.tpeOpt.get))
          case _ =>
        }
      case _ =>
    }
    return AST.MTransformer.PreResultExpEta
  }

  override def preExpSelect(o: AST.Exp.Select): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, o.receiverOpt)
      case _ =>
    }
    return AST.MTransformer.PreResultExpSelect
  }

  override def preExpInvoke(o: AST.Exp.Invoke): AST.MTransformer.PreResult[AST.Exp] = {
    if (o.id.value == string"apply") {
      return AST.MTransformer.PreResultExpInvoke
    }
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, o.receiverOpt)
      case _ =>
    }

    return AST.MTransformer.PreResultExpInvoke
  }

  override def preExpInvokeNamed(o: AST.Exp.InvokeNamed): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, o.receiverOpt)
      case _ =>
    }
    return AST.MTransformer.PreResultExpInvokeNamed
  }

  override def preExpFun(o: AST.Exp.Fun): AST.MTransformer.PreResult[AST.Exp] = {
    halt("TODO") // TODO
  }

  override def postTyped(o: AST.Typed): MOption[AST.Typed] = {
    addType(o)
    return MNone()
  }

  override def postTypedFun(o: AST.Typed.Fun): MOption[AST.Typed] = {
    addType(o)
    return MNone()
  }

}
