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

  @datatype class Config(
    defaultBitWidth: Z,
    defaultStringSize: Z,
    defaultArraySize: Z,
    customArraySizes: HashMap[AST.Typed, Z]
  )

  @datatype class Header(headers: HashMap[String, ISZ[ST]], footer: ISZ[ST])

  @datatype class Impl(impls: HashMap[String, ISZ[ST]], footer: ISZ[ST])

  @datatype class CClass(header: Header, impl: Impl)

  val transKind: String = "Static C Transpiler"
  val unitType: ST = st"void"
  val bType: ST = st"B"
  val zType: ST = st"Z"
  val empty: ST = st""
  val trueLit: ST = st"T"
  val falseLit: ST = st"T"
  val i8Min: Z = conversions.Z8.toZ(Z8.Min)
  val i16Min: Z = conversions.Z16.toZ(Z16.Min)
  val i32Min: Z = conversions.Z32.toZ(Z32.Min)
  val i64Min: Z = conversions.Z64.toZ(Z64.Min)
  val i8Max: Z = conversions.Z8.toZ(Z8.Max)
  val i16Max: Z = conversions.Z16.toZ(Z16.Max)
  val i32Max: Z = conversions.Z32.toZ(Z32.Max)
  val i64Max: Z = conversions.Z64.toZ(Z64.Max)
  val u8Max: Z = conversions.N8.toZ(N8.Max)
  val u16Max: Z = conversions.N16.toZ(N16.Max)
  val u32Max: Z = conversions.N32.toZ(N32.Max)
  val u64Max: Z = conversions.N64.toZ(N64.Max)
  val stringTypeId: U32 = Fingerprint.u32(dotName(AST.Typed.string.ids))

  @pure def dotName(ids: QName): String = {
    return st"${(ids, ".")}".render
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

  @pure def encodeName(id: String): String = {
    return id // TODO
  }
}

import StaticTranspiler._

@record class StaticTranspiler(config: Config, th: TypeHierarchy, reporter: Reporter) {

  var nameMap: HashMap[String, QName] = HashMap.empty

  var cclassMap: HashMap[QName, CClass] = HashMap.empty

  def transpileObjectMethod(method: QName, substMap: SubstMap): Unit = {
    val methodInfo: Info.Method = th.nameMap.get(method) match {
      case Some(info: Info.Method) => info
      case _ =>
        reporter.error(None(), transKind, st"'${dotName(method)}' is not a method".render)
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

    def freshTemp(isClone: B, t: ST, expOpt: Option[ST]): ST = {
      val p = freshTempName(nextTempNum)
      nextTempNum = p._2
      val rhs: ST = expOpt match {
        case Some(e) => if (isClone) st" = *($e)" else st" = $e"
        case _ => st""
      }
      stmts = stmts :+ st"$t ${p._1}$rhs;"
      return p._1
    }

    @pure def transLitB(exp: AST.Exp.LitB): ST = {
      return if (exp.value) trueLit else falseLit
    }

    @pure def transLitC(exp: AST.Exp.LitC): ST = {
      halt("TODO") // TODO
    }

    @pure def transLitZ(exp: AST.Exp.LitZ): ST = {
      val n = exp.value
      var ok = T
      config.defaultBitWidth match {
        case z"8" =>
          if (!(i8Min <= n && n <= i8Max)) {
            ok = F
          }
        case z"16" =>
          if (!(i16Min <= n && n <= i16Max)) {
            ok = F
          }
        case z"32" =>
          if (!(i32Min <= n && n <= i32Max)) {
            ok = F
          }
        case z"64" =>
          if (!(i64Min <= n && n <= i64Max)) {
            ok = F
          }
        case _ => halt("Infeasible")
      }
      if (!ok) {
        reporter.error(exp.posOpt, transKind, s"Invalid ${config.defaultBitWidth}-bit Z literal '$n'.")
      }
      return st"Z_C($n)"
    }

    @pure def transLitF32(exp: AST.Exp.LitF32): ST = {
      return st"${exp.value.string}F"
    }

    @pure def transLitF64(exp: AST.Exp.LitF64): ST = {
      return st"${exp.value.string}"
    }

    @pure def transLitR(exp: AST.Exp.LitR): ST = {
      return st"${exp.string}L"
    }

    @pure def transLitString(exp: AST.Exp.LitString): ST = {
      def escape(c: C): String = {
        if (c <= '\u00FF') {
          c.native match {
            case '\u0000' => return "\\0"
            case '\'' => return "\\'"
            case '\u0022' => return "\\\\u0022"
            case '?' => return "\\?"
            case '\\' => return "\\\\"
            case '\u0007' => return "\\a"
            case '\b' => return "\\b"
            case '\f' => return "\\f"
            case '\n' => return "\\n"
            case '\r' => return "\\r"
            case '\t' => return "\\t"
            case '\u000B' => return "\\v"
            case _ =>
              if ('\u0032' <= c && c < '\u007F') {
                return c.string
              } else {
                return s"\\X${ops.COps.hex2c(c >>> '\u0004')}${ops.COps.hex2c(c & '\u000F')}"
              }
          }
        } else {
          reporter.error(exp.posOpt, transKind, "Static C translation does not support Unicode string.")
          return "\\?"
        }
      }
      val value = st""""${(conversions.String.toCis(exp.value).map(escape), "")}"""".render
      val temp = freshTemp(F, st"", Some(st"""(String) { .size = Z_C(${value.size}), .value = "$value" }"""))
      return temp
    }

    @pure def transSubZLit(exp: AST.Exp.StringInterpolate): ST = {
      halt("TODO") // TODO
    }

    def transBinary(exp: AST.Exp.Binary): ST = {
      halt("TODO") // TODO
    }

    def transUnary(exp: AST.Exp.Unary): ST = {
      halt("TODO") // TODO
    }

    def isSubZLit(exp: AST.Exp.StringInterpolate): B = {
      exp.attr.typedOpt.get match {
        case t: AST.Typed.Name if t.args.isEmpty =>
          th.typeMap.get(t.ids) match {
            case Some(_: TypeInfo.SubZ) => return T
            case _ => return F
          }
        case _ => return F
      }
    }

    val expST: ST = exp match {
      case exp: AST.Exp.LitB => val r = transLitB(exp); r
      case exp: AST.Exp.LitC => val r = transLitC(exp); r
      case exp: AST.Exp.LitZ => val r = transLitZ(exp); r
      case exp: AST.Exp.LitF32 => val r = transLitF32(exp); r
      case exp: AST.Exp.LitF64 => val r = transLitF64(exp); r
      case exp: AST.Exp.LitR => val r = transLitR(exp); r
      case exp: AST.Exp.StringInterpolate if isSubZLit(exp) => val r = transSubZLit(exp); r
      case exp: AST.Exp.LitString => val r = transLitString(exp); r
      case exp: AST.Exp.Binary => val r = transBinary(exp); r
      case exp: AST.Exp.Unary => val r = transUnary(exp); r
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

    def freshTemp(isClone: B, t: ST, expOpt: Option[ST]): ST = {
      val p = freshTempName(nextTempNum)
      nextTempNum = p._2
      val rhs: ST = expOpt match {
        case Some(e) => if (isClone) st" = *($e)" else st" = $e"
        case _ => st""
      }
      stmts = stmts :+ st"$t ${p._1}$rhs;"
      return p._1
    }

    def transVar(stmt: AST.Stmt.Var, init: AST.Stmt.Expr): Unit = {
      val t: AST.Typed = stmt.tipeOpt match {
        case Some(tipe) => tipe.typedOpt.get
        case _ => init.typedOpt.get
      }
      if (isScalar(t)) {
        stmts = stmts :+ st"${transpileType(t, F)} ${localName(stmt.id.value)} = ${transExp(init.exp)};"
      } else {
        val temp = freshTemp(T, transpileType(t, F), Some(st"${transExp(init.exp)}"))
        stmts = stmts :+ st"${transpileType(t, T)} ${localName(stmt.id.value)} = &$temp;"
      }
    }

    def transVarComplex(stmt: AST.Stmt.Var): Unit = {
      halt("TODO") // TODO
    }

    def transAssert(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transAssume(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transCprint(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transCprintln(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transEprint(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transEprintln(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transPrint(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transPrintln(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def transHalt(exp: AST.Exp.Invoke): Unit = {
      halt("TODO") // TODO
    }

    def isBuiltInStmt(exp: AST.Exp.Invoke): B = {
      exp.attr.resOpt match {
        case Some(AST.ResolvedInfo.BuiltIn(kind)) =>
          kind match {
            case AST.ResolvedInfo.BuiltIn.Kind.Assert => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Assume => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Cprint => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Cprintln => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Eprint => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Eprintln => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Print => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Println => return T
            case AST.ResolvedInfo.BuiltIn.Kind.Halt => return T
            case _ => return F
          }
        case _ => return F
      }
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
      case AST.Stmt.Expr(exp: AST.Exp.Invoke) if isBuiltInStmt(exp) =>
        exp.attr.resOpt.get match {
          case AST.ResolvedInfo.BuiltIn(kind) =>
            kind match {
              case AST.ResolvedInfo.BuiltIn.Kind.Assert => transAssert(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Assume => transAssume(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Cprint => transCprint(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Cprintln => transCprintln(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Eprint => transEprint(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Eprintln => transEprintln(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Print => transPrint(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Println => transPrintln(exp)
              case AST.ResolvedInfo.BuiltIn.Kind.Halt => transHalt(exp)
              case _ => halt("Infeasible")
            }
          case _ => halt("Infeasible")
        }
      case _ => halt("TODO") // TODO
    }

    return stmts
  }

  @pure def methodHeader(method: AST.ResolvedInfo.Method): ST = {
    val name = methodName(method)
    val tpe = method.tpeOpt.get
    val retType = transpileType(tpe.ret, F)
    val params: ST =
      if (method.paramNames.isEmpty)
        unitType
      else
        st"${(
          for (p <- ops.ISZOps(tpe.args).zip(method.paramNames))
            yield st"${transpileType(p._1, T)} ${localName(p._2)}",
          ", "
        )}"
    return st"$retType $name($params)"
  }

  @pure def transpileType(tpe: AST.Typed, isPtr: B): ST = {
    tpe match {
      case AST.Typed.unit => return unitType
      case AST.Typed.b => return bType
      case AST.Typed.z => return zType
      case _ => halt("TODO") // TODO
    }
  }

  @pure def isScalar(t: AST.Typed): B = {
    t match {
      case AST.Typed.b =>
      case AST.Typed.c =>
      case AST.Typed.z =>
      case AST.Typed.f32 =>
      case AST.Typed.f64 =>
      case AST.Typed.r =>
      case t: AST.Typed.Name if t.args.isEmpty =>
        th.typeMap.get(t.ids) match {
          case Some(_: TypeInfo.SubZ) =>
          case _ => return F
        }
      case _ => return F
    }
    return T
  }
}
