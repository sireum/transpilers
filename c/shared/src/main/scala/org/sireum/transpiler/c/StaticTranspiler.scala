// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.message._

import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver._
import org.sireum.lang.tipe._
import org.sireum.transpiler.c.util.Fingerprint

object StaticTranspiler {

  type SubstMap = HashMap[AST.Typed.TypeVar, AST.Typed]

  @datatype class Config(defaultArraySize: Z, customArraySizes: HashMap[AST.Typed, Z])

  @datatype class Header(headers: HashMap[String, ISZ[ST]], footer: ISZ[ST])

  @datatype class Impl(impls: HashMap[String, ISZ[ST]], footer: ISZ[ST])

  @datatype class CClass(header: Header, impl: Impl)

  val kind: String = "Static C Transpiler"
  val unitType: ST = st"void"
  val bType: ST = st"B"
  val zType: ST = st"Z"
  val empty: ST = st""
}

import StaticTranspiler._

@record class StaticTranspiler(config: Config, th: TypeHierarchy, reporter: Reporter) {

  var nameMap: HashMap[String, QName] = HashMap.empty

  var cclassMap: HashMap[QName, CClass] = HashMap.empty

  def transpileObjectMethod(method: QName, substMap: SubstMap): Unit = {
    val methodInfo: Info.Method = th.nameMap.get(method) match {
      case Some(info: Info.Method) => info
      case _ =>
        reporter.error(None(), kind, st"'${dotName(method)}' is not a method".render)
        return
    }
    assert(methodInfo.ast.sig.typeParams.isEmpty) // TODO: Specialize object method
    transpileMethod(methodInfo.ast)
  }

  def transpileMethod(method: AST.Stmt.Method): Unit = {
    val info = methodResolvedInfo(method)
    val header = methodHeader(info)
    val stmts: ISZ[ST] = for (stmt <- method.bodyOpt.get.stmts; st <- transpileStmt(stmt)) yield st
    val impl =
      st"""$header {
      |  ${(stmts, "\n")}
      |}"""
    val owner = info.owner
    val id = info.name
    val cclass: CClass = cclassMap.get(owner) match {
      case Some(cc) => cc
      case _ => CClass(Header(HashMap.empty, ISZ()), Impl(HashMap.empty, ISZ()))
    }
    val (headers, impls): (ISZ[ST], ISZ[ST]) = cclass.header.headers.get(id) match {
      case Some(s) => (s, cclass.impl.impls.get(id).get)
      case _ => (ISZ(), ISZ())
    }
    cclassMap = cclassMap + owner ~> cclass(
      header = cclass.header(headers = cclass.header.headers + id ~> (headers :+ st"$header;")),
      impl = cclass.impl(impls = cclass.impl.impls + id ~> (impls :+ impl))
    )
  }

  @pure def transpileExp(exp: AST.Exp, _nextTempNum: Z): (ST, Z, ISZ[ST]) = {
    var stmts = ISZ[ST]()
    var nextTempNum = _nextTempNum

    def freshTemp(isClone: B, tpe: AST.Typed, expOpt: Option[ST]): ST = {
      val p = freshTempName(nextTempNum)
      nextTempNum = p._2
      val rhs: ST = expOpt match {
        case Some(e) => if (isClone) st" = *($e)" else st" = $e"
        case _ => st""
      }
      stmts = stmts :+ st"${transpileType(tpe, isClone)} ${p._1}$rhs;"
      return p._1
    }

    def transLitB(exp: AST.Exp.LitB): ST = {
      return st"?" // TODO
    }

    def transLitZ(exp: AST.Exp.LitZ): ST = {
      return st"?" // TODO
    }

    def transBinary(exp: AST.Exp.Binary): ST = {
      return st"?" // TODO
    }

    def transUnary(exp: AST.Exp.Unary): ST = {
      return st"?" // TODO
    }

    def transInvoke(exp: AST.Exp.Invoke): ST = {
      return st"?" // TODO
    }

    val expST: ST = exp match {
      case exp: AST.Exp.LitB => val r = transLitB(exp); r
      case exp: AST.Exp.LitZ => val r = transLitZ(exp); r
      case exp: AST.Exp.Binary => val r = transBinary(exp); r
      case exp: AST.Exp.Unary => val r = transUnary(exp); r
      case exp: AST.Exp.Invoke => val r = transInvoke(exp); r
      case _ => halt("TODO") // TODO
    }

    return (expST, nextTempNum, stmts)
  }

  @pure def transpileStmt(stmt: AST.Stmt): ISZ[ST] = {
    var stmts = ISZ[ST]()
    var nextTempNum = z"0"

    def transExp(exp: AST.Exp): ST = {
      val t = transpileExp(exp, nextTempNum)
      nextTempNum = t._2
      stmts = stmts ++ t._3
      return t._1
    }

    def freshTemp(isClone: B, tpe: AST.Typed, expOpt: Option[ST]): ST = {
      val p = freshTempName(nextTempNum)
      nextTempNum = p._2
      val rhs: ST = expOpt match {
        case Some(e) => if (isClone) st" = *($e)" else st" = $e"
        case _ => st""
      }
      stmts = stmts :+ st"${transpileType(tpe, isClone)} ${p._1}$rhs;"
      return p._1
    }

    def transVar(stmt: AST.Stmt.Var, init: AST.Stmt.Expr): Unit = {
      val t: AST.Typed = stmt.tipeOpt match {
        case Some(tipe) => tipe.typedOpt.get
        case _ => init.typedOpt.get
      }
      val isClone = isMutable(t)
      val rhs = transExp(init.exp)
      stmts = stmts :+ st"${transpileType(t, isClone)} ${localName(stmt.id.value)} = ${if (isClone) st"*($rhs)" else rhs};"
    }

    def transVarComplex(stmt: AST.Stmt.Var): Unit = {
      halt("TODO") // TODO
    }

    stmts = stmts :+ empty
    stmt.posOpt match {
      case Some(pos) => stmts = stmts :+ st"// L${pos.beginLine}"
      case _ => stmts = stmts :+ st"// L?"
    }
    stmt match {
      case stmt: AST.Stmt.Var =>
        stmt.initOpt.get match {
          case init: AST.Stmt.Expr => transVar(stmt, init)
          case _ => transVarComplex(stmt)
        }
      case _ => halt("TODO") // TODO
    }

    return stmts
  }

  @pure def isMutable(t: AST.Typed): B = {
    t match {
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.msName) {
          return T
        } else {
          th.typeMap.get(t.ids) match {
            case Some(info: TypeInfo.Sig) => return !info.ast.isImmutable
            case Some(info: TypeInfo.AbstractDatatype) => return !info.ast.isDatatype
            case _ => return F
          }
        }
      case t: AST.Typed.Tuple =>
        var i = 0
        val sz = t.args.size
        while (i < sz) {
          if (isMutable(t.args(i))) {
            return T
          }
          i = i + 1
        }
        return F
      case _ => return F
    }
  }

  @pure def methodHeader(method: AST.ResolvedInfo.Method): ST = {
    val name = methodName(method)
    val tpe = method.tpeOpt.get
    val retType = transpileType(tpe.ret, T)
    val params: ST =
      if (method.paramNames.isEmpty)
        unitType
      else
        st"${(
          for (p <- ops.ISZOps(tpe.args).zip(method.paramNames))
            yield st"${transpileType(p._1, F)} ${localName(p._2)}",
          ", "
        )}"
    return st"$retType $name($params)"
  }

  @pure def transpileType(tpe: AST.Typed, isLValue: B): ST = {
    tpe match {
      case AST.Typed.unit => return unitType
      case AST.Typed.b => return bType
      case AST.Typed.z => return zType
      case _ => halt("TODO") // TODO
    }
  }

  @pure def freshTempName(nextTempNum: Z): (ST, Z) = {
    return (st"t_$nextTempNum", nextTempNum + 1)
  }

  @pure def methodResolvedInfo(method: AST.Stmt.Method): AST.ResolvedInfo.Method = {
    method.attr.resOpt.get match {
      case res: AST.ResolvedInfo.Method => return res
      case _ => halt("Infeasible")
    }
  }

  @pure def methodName(method: AST.ResolvedInfo.Method): String = {
    val tpe = method.tpeOpt.get
    val ids = method.owner :+ method.name :+ Fingerprint.string(tpe.string)
    return underscoreName(if (method.isInObject) ids else "I" +: ids)
  }

  @pure def localName(id: String): String = {
    return st"l_${encodeName(id)}".render
  }

  @pure def underscoreName(ids: QName): String = {
    return st"${(ids.map(encodeName), "_")}".render
  }

  @pure def dotName(ids: QName): String = {
    return st"${(ids, ".")}".render
  }

  @pure def encodeName(id: String): String = {
    return id // TODO
  }
}
