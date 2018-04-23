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
      impl = cclass.impl(impls = cclass.impl.impls + id ~> (impls :+ impl)))
  }

  @pure def transpileStmt(stmt: AST.Stmt): ISZ[ST] = {
    return ISZ() // TODO
  }

  @pure def methodHeader(method: AST.ResolvedInfo.Method): ST = {
    val name = methodName(method)
    val tpe = method.tpeOpt.get
    val retType = transpileType(tpe.ret)
    val params: ST =
      if (method.paramNames.isEmpty)
        unitType
      else
        st"${(
          for (p <- ops.ISZOps(tpe.args).zip(method.paramNames))
            yield st"${transpileType(p._1)} ${localName(p._2)}",
          ", "
        )}"
    return st"$retType $name($params)"
  }

  @pure def transpileType(tpe: AST.Typed): ST = {
    tpe match {
      case AST.Typed.unit => return unitType
      case AST.Typed.b => return bType
      case AST.Typed.z => return zType
      case _ => halt("TODO") // TODO
    }
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
