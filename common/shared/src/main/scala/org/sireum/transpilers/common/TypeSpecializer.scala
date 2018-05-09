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

  @datatype class Result(
    typeHierarchy: TypeHierarchy,
    entryPoints: ISZ[EntryPoint],
    nameTypes: HashMap[QName, HashSet[AST.Typed.Name]],
    otherTypes: HashSet[AST.Typed],
    objectVars: HashSet[QName],
    methods: HashMap[QName, HashSet[Method]],
    funs: HashMap[QName, HashSet[Fun]]
  )

  @datatype class Method(
    isInObject: B,
    isNested: B,
    owner: QName,
    id: String,
    ast: AST.Stmt.Method,
    closureEnv: HashMap[String, AST.Typed]
  )

  @datatype class Fun(ast: AST.Exp.Fun)

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

  @datatype class SMethod(receiverOpt: Option[AST.Typed.Name], res: AST.ResolvedInfo.Method)

  val tsKind: String = "Type Specializer"
  val continuePreResult: AST.MTransformer.PreResult[AST.Exp] = AST.MTransformer.PreResult(T, MNone())
  val stopPreResult: AST.MTransformer.PreResult[AST.Exp] = AST.MTransformer.PreResult(F, MNone())

  def specialize(th: TypeHierarchy, entryPoints: ISZ[EntryPoint], reporter: Reporter): Result = {
    val r = TypeSpecializer(th, entryPoints).specialize()
    return r
  }

}

import TypeSpecializer._

@record class TypeSpecializer(th: TypeHierarchy, eps: ISZ[EntryPoint]) extends AST.MTransformer {
  val reporter: Reporter = Reporter.create
  val methodRefinement: Poset[CallGraph.Node] = CallGraph.methodRefinements(th)
  var nameTypes: HashMap[QName, HashSet[AST.Typed.Name]] = HashMap.empty
  var otherTypes: HashSet[AST.Typed] = HashSet.empty
  var objectVars: HashSet[QName] = HashSet.empty
  var methods: HashMap[QName, HashSet[Method]] = HashMap.empty
  var funs: HashMap[QName, HashSet[Fun]] = HashMap.empty
  var traitMethods: ISZ[SMethod] = ISZ()
  var workList: ISZ[Info.Method] = ISZ()
  var seen: HashSet[SMethod] = HashSet.empty
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

    for (ep <- eps) {
      ep match {
        case ep: EntryPoint.Method => entryMethod(ep)
        case ep: EntryPoint.Worksheet => entryWorksheet(ep)
      }
    }
    return TypeSpecializer.Result(th, eps, nameTypes, otherTypes, objectVars, methods, funs)
  }

  override def postResolvedAttr(o: AST.ResolvedAttr): MOption[AST.ResolvedAttr] = {
    o.resOpt.get match {
      case res: AST.ResolvedInfo.Var =>
        if (res.isInObject && !res.isSpec) {
          objectVars = objectVars + (res.owner :+ res.id)
        }
      case res: AST.ResolvedInfo.LocalVar =>
        if (res.scope == AST.ResolvedInfo.LocalVar.Scope.Closure) {
          halt("TODO") // TODO
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
    return MNone()
  }

  @pure def substMethod(m: Info.Method, substMap: HashMap[String, AST.Typed]): Info.Method = {
    if (substMap.nonEmpty) {
      val newAst = TypeSubstitutor(substMap).transformStmt(m.ast).asInstanceOf[AST.Stmt.Method]
      return m(ast = newAst)
    } else {
      return m
    }
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
    val asm = TypeChecker.buildTypeSubstMap(info.name, posOpt, info.ast.typeParams, receiver.args, reporter).get
    val m = info.methods.get(method.res.id).get
    val mFun = m.ast.attr.resOpt.get.asInstanceOf[AST.ResolvedInfo.Method].tpeOpt.get
    val msm = TypeChecker.unify(th, posOpt, TypeRelation.Equal, method.res.tpeOpt.get, mFun, reporter).get
    val sm = combine(asm, msm)
    if (m.ast.bodyOpt.nonEmpty) {
      return substMethod(m, sm)
    }
    halt("TODO") // TODO
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
        val m = th.nameMap.get(method.res.owner :+ method.res.id).get.asInstanceOf[Info.Method]
        seen = seen + method
        if (m.ast.sig.typeParams.isEmpty) {
          workList = workList :+ m
          return
        }
        val mType = m.typedOpt.get.asInstanceOf[AST.Typed.Method].tpe
        val substMapOpt = TypeChecker.unifyMethod(tsKind, th, posOpt, method.res.tpeOpt.get, mType, reporter)
        substMapOpt match {
          case Some(substMap) => workList = workList :+ substMethod(m, substMap)
          case _ =>
        }
    }
    return
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
    addSMethod(posOpt, SMethod(rOpt, m))
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
              return continuePreResult
            }
            addSMethod(ref.posOpt, SMethod(rOpt, m))
          case _ =>
        }
      case _ =>
    }
    return continuePreResult
  }

  override def preExpSelect(o: AST.Exp.Select): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, o.receiverOpt)
      case _ =>
    }
    return continuePreResult
  }

  override def preExpInvoke(o: AST.Exp.Invoke): AST.MTransformer.PreResult[AST.Exp] = {
    if (o.id.value == string"apply") {
      return continuePreResult
    }
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, o.receiverOpt)
      case _ =>
    }

    return continuePreResult
  }

  override def preExpInvokeNamed(o: AST.Exp.InvokeNamed): AST.MTransformer.PreResult[AST.Exp] = {
    o.attr.resOpt.get match {
      case m: AST.ResolvedInfo.Method => addResolvedMethod(o.posOpt, m, o.receiverOpt)
      case _ =>
    }
    return continuePreResult
  }

  def addType(o: AST.Typed): Unit = {
    o match {
      case o: AST.Typed.Name =>
        val set: HashSet[AST.Typed.Name] = nameTypes.get(o.ids) match {
          case Some(s) => s
          case _ => HashSet.empty
        }
        nameTypes = nameTypes + o.ids ~> (set + o)
        return
      case _: AST.Typed.Enum => otherTypes = otherTypes + o
      case _: AST.Typed.Tuple => otherTypes = otherTypes + o
      case _: AST.Typed.Object => // skip
      case _: AST.Typed.Fun => // skip
      case _: AST.Typed.Method => // skip
      case _: AST.Typed.Methods => // skip
      case _: AST.Typed.Package => // skip
      case _: AST.Typed.TypeVar => halt("Infeasible")
    }
    return
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
