// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.message._

import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver.QName
import org.sireum.lang.tipe._
import org.sireum.transpiler.c.util.Fingerprint

object StaticTranspiler {

  @enum object TypeKind {
    'Scalar
    'ImmutableTrait
    'Immutable
    'MutableTrait
    'Mutable
    'IS
    'MS
  }

  type SubstMap = HashMap[AST.Typed.TypeVar, AST.Typed]

  @datatype class Config(
    lineNumber: B,
    fprintWidth: Z,
    defaultBitWidth: Z,
    defaultStringSize: Z,
    defaultArraySize: Z,
    customArraySizes: HashMap[AST.Typed, Z]
  )

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
  val abort: ST = st"abort();"

  @pure def dotName(ids: QName): String = {
    return st"${(ids, ".")}".render
  }

  @pure def methodResolvedInfo(method: AST.Stmt.Method): AST.ResolvedInfo.Method = {
    method.attr.resOpt.get match {
      case res: AST.ResolvedInfo.Method => return res
      case _ => halt("Infeasible")
    }
  }

  @pure def localName(id: String): String = {
    return st"l_${encodeName(id)}".render
  }

  @pure def mangleName(ids: QName): String = {
    val r: ST =
      if (ids.size == z"1") st"top_${ids(0)}"
      else if (ids.size >= 2 && ids(0) == string"org" && ids(1) == string"sireum")
        st"${(ops.ISZOps(ids).drop(2).map(encodeName), "_")}"
      else st"${(ids.map(encodeName), "_")}"
    return r.render
  }

  @pure def encodeName(id: String): String = {
    return id // TODO
  }

  @pure def typeName(tOpt: Option[AST.Typed]): QName = {
    tOpt.get match {
      case t: AST.Typed.Name => return t.ids
      case _ => halt("Infeasible")
    }
  }

  @pure def filename(uriOpt: Option[String]): String = {
    uriOpt match {
      case Some(uri) =>
        val i = ops.StringOps(uri).lastIndexOf('/')
        if (i < 0) {
          return uri
        }
        return ops.StringOps(uri).substring(i + 1, uri.size)
      case _ => return "<no-uri>"
    }
  }

  @pure def filenameOfPosOpt(posOpt: Option[Position]): String = {
    posOpt match {
      case Some(pos) => return filename(pos.uriOpt)
      case _ => return filename(None())
    }
  }
}

import StaticTranspiler._

@record class StaticTranspiler(config: Config, th: TypeHierarchy, reporter: Reporter) {

  var genTypeHeader: ISZ[ST] = ISZ()
  var genHeader: ISZ[ST] = ISZ()
  var genImpl: ISZ[ST] = ISZ()

  var stmts: ISZ[ST] = ISZ()

  var nextTempNum: Z = 0

  def transpileWorksheet(program: AST.TopUnit.Program): ST = {
    stmts = ISZ(st"""DeclNewStackFrame(NULL, "${filename(program.fileUriOpt)}", "", "<main>", 0);""")
    nextTempNum = 0
    assert(program.packageName.ids.isEmpty)
    for (stmt <- program.body.stmts) {
      transpileStmt(stmt)
    }
    val r =
      st"""#include <gen.h>
      |
      |int main(void) {
      |  ${(stmts, "\n")}
      |  return 0;
      |}"""

    return r
  }

  def transpileObjectMethod(method: QName, substMap: SubstMap): Unit = {
    val methodInfo: Info.Method = th.nameMap.get(method) match {
      case Some(info: Info.Method) => info
      case _ =>
        reporter.error(None(), transKind, st"'${dotName(method)}' is not a method".render)
        return
    }
    assert(methodInfo.ast.sig.typeParams.isEmpty && substMap.isEmpty) // TODO: Specialize object method
    transpileMethod(methodInfo.ast)
  }

  def transpileMethod(method: AST.Stmt.Method): Unit = {
    val oldNextTempNum = nextTempNum
    val oldStmts = stmts

    nextTempNum = 0
    stmts = ISZ()

    val info = methodResolvedInfo(method)
    val header = methodHeader(info)
    for (stmt <- method.bodyOpt.get.stmts) {
      transpileStmt(stmt)
    }
    val impl =
      st"""$header {
      |  DeclNewStackFrame(caller, "${filenameOfPosOpt(method.posOpt)}", "${dotName(info.owner)}", "${info.name}", 0);
      |  ${(stmts, "\n")}
      |}"""

    genHeader = genHeader :+ st"$header;"
    genImpl = genImpl :+ impl

    nextTempNum = oldNextTempNum
    stmts = oldStmts
  }

  @pure def transpileExp(exp: AST.Exp): ST = {

    @pure def transLitB(exp: AST.Exp.LitB): ST = {
      return if (exp.value) trueLit else falseLit
    }

    @pure def transLitC(exp: AST.Exp.LitC): ST = {
      return st"'${escapeChar(exp.posOpt, exp.value)}'"
    }

    def checkBitWidth(n: Z, bitWidth: Z): Unit = {
      var ok = T
      val bw: Z = if (bitWidth == z"0") config.defaultBitWidth else bitWidth
      bw match {
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
    }

    def transLitZ(exp: AST.Exp.LitZ): ST = {
      val n = exp.value
      checkBitWidth(n, 0)
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
      val cis = conversions.String.toCis(exp.value)
      val value = MSZ.create[String](cis.size, "")
      val posOpt = exp.posOpt
      var i = 0
      while (i < cis.size) {
        value(i) = escapeChar(posOpt, cis(i))
        i = i + 1
      }
      return st"""string("${(value, "")}")"""
    }

    @pure def transSubZLit(exp: AST.Exp.StringInterpolate): ST = {
      val tname = typeName(exp.attr.typedOpt)
      val info: TypeInfo.SubZ = th.typeMap.get(tname).get match {
        case inf: TypeInfo.SubZ => inf
        case _ => halt("Infeasible")
      }
      val n = Z(exp.lits(0).value).get
      checkBitWidth(n, info.ast.bitWidth)
      return st"${(dotName(tname), "")}_C"
    }

    def transBinary(exp: AST.Exp.Binary): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.BuiltIn =>
          val tname = typeName(exp.attr.typedOpt)
          res.kind match {
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryImply =>
              return st"(!(${transpileExp(exp.left)}) || ${transpileExp(exp.right)})"
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryCondAnd =>
              return st"(${transpileExp(exp.left)} && ${transpileExp(exp.right)})"
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryCondOr =>
              return st"(${transpileExp(exp.left)} || ${transpileExp(exp.right)})"
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryMapsTo => halt("TODO") // TODO
            case _ =>
              val op: String = res.kind match {
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAdd => "_add"
                case AST.ResolvedInfo.BuiltIn.Kind.BinarySub => "_sub"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryMul => "_mul"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryDiv => "_div"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryRem => "_rem"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryEq => "_eq"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryNe => "_ne"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryLt => "_lt"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryLe => "_le"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryGt => "_gt"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryGe => "_ge"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryShl => "_shl"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryShr => "_shr"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryUshr => "_ushr"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAnd => "_and"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryOr => "_or"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryXor => "_xor"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAppend => "_append"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryPrepend => "_prepend"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAppendAll => "_appendall"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryRemoveAll => "_removeall"
                case _ => halt("TODO") // TODO
              }
              return st"${mangleName(tname)}$op(${transpileExp(exp.left)}, ${transpileExp(exp.right)})"
          }
        case _ => halt("TODO") // TODO
      }
    }

    def transUnary(exp: AST.Exp.Unary): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.BuiltIn =>
          if (res.kind == AST.ResolvedInfo.BuiltIn.Kind.UnaryNot) {
            return st"!${transpileExp(exp.exp)}"
          } else {
            val tname = typeName(exp.typedOpt)
            val op: String = res.kind match {
              case AST.ResolvedInfo.BuiltIn.Kind.UnaryComplement => "_complement"
              case AST.ResolvedInfo.BuiltIn.Kind.UnaryPlus => "_plus"
              case AST.ResolvedInfo.BuiltIn.Kind.UnaryMinus => "_minus"
              case _ => halt("Infeasible")
            }
            return st"${mangleName(tname)}$op(${transpileExp(exp.exp)})"
          }
        case _ => halt("TODO") // TODO
      }
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

    exp match {
      case exp: AST.Exp.LitB => val r = transLitB(exp); return r
      case exp: AST.Exp.LitC => val r = transLitC(exp); return r
      case exp: AST.Exp.LitZ => val r = transLitZ(exp); return r
      case exp: AST.Exp.LitF32 => val r = transLitF32(exp); return r
      case exp: AST.Exp.LitF64 => val r = transLitF64(exp); return r
      case exp: AST.Exp.LitR => val r = transLitR(exp); return r
      case exp: AST.Exp.StringInterpolate if isSubZLit(exp) => val r = transSubZLit(exp); return r
      case exp: AST.Exp.LitString => val r = transLitString(exp); return r
      case exp: AST.Exp.Binary => val r = transBinary(exp); return r
      case exp: AST.Exp.Unary => val r = transUnary(exp); return r
      case _ => halt("TODO") // TODO
    }
  }

  def transToString(s: ST, exp: AST.Exp): Unit = {
    // TODO: Gen string on demand
    val tmp = transpileExp(exp)
    exp.typedOpt.get match {
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.isName || t.ids == AST.Typed.msName) {
          stmts = stmts :+ st"A_string_${Fingerprint.string(t.args(0).string, config.fprintWidth)}(sf, $tmp, $s);"
        } else if (t.args.isEmpty) {
          stmts = stmts :+ st"${mangleName(t.ids)}_string($tmp, $s);"
        } else {
          stmts = stmts :+ st"${mangleName(t.ids)}_string_${Fingerprint.string(t.args.string, config.fprintWidth)}(sf, $tmp, $s);"
        }
      case t: AST.Typed.Tuple =>
        stmts = stmts :+ st"Tuple${t.args.size}_string_${Fingerprint.string(t.args.string, config.fprintWidth)}(sf, $tmp, $s);"
      case t: AST.Typed.Enum =>
        stmts = stmts :+ st"${mangleName(t.name)}_string(sf, $tmp, $s);"
      case t: AST.Typed.Fun =>
        stmts = stmts :+ st"Fun_string_${Fingerprint.string(t.string, config.fprintWidth)}(sf, $tmp, $s);"
      case _ => halt("Infeasible")
    }
  }

  def transPrintH(isOut: ST, exp: AST.Exp): Unit = {
    // TODO: Gen print on demand
    val tmp = transpileExp(exp)
    exp.typedOpt.get match {
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.isName || t.ids == AST.Typed.msName) {
          stmts = stmts :+ st"A_print_${Fingerprint.string(t.args(0).string, config.fprintWidth)}($tmp);"
        } else if (t.args.isEmpty) {
          stmts = stmts :+ st"${mangleName(t.ids)}_print($tmp);"
        } else {
          stmts = stmts :+ st"${mangleName(t.ids)}_print_${Fingerprint.string(t.args.string, config.fprintWidth)}($tmp);"
        }
      case t: AST.Typed.Tuple =>
        stmts = stmts :+ st"Tuple${t.args.size}_print_${Fingerprint.string(t.args.string, config.fprintWidth)}($tmp);"
      case t: AST.Typed.Enum =>
        stmts = stmts :+ st"${mangleName(t.name)}_print($tmp);"
      case t: AST.Typed.Fun =>
        stmts = stmts :+ st"Fun_print_${Fingerprint.string(t.string, config.fprintWidth)}($tmp);"
      case _ => halt("Infeasible")
    }
  }

  def transpileVarPattern(stmt: AST.Stmt.VarPattern): Unit = {
    halt("TODO") // TODO
  }

  def transpileBlock(stmt: AST.Stmt.Block): Unit = {
    val oldStmts = stmts
    stmts = ISZ()
    for (stmt <- stmt.body.stmts) {
      transpileStmt(stmt)
    }
    stmts = oldStmts :+
      st"""{
      |  ${(stmts, "\n")}
      |}"""
  }

  def transpileIf(stmt: AST.Stmt.If): Unit = {
    val cond = transpileExp(stmt.cond)
    val oldStmts = stmts
    stmts = ISZ()
    for (stmt <- stmt.thenBody.stmts) {
      transpileStmt(stmt)
    }
    if (stmt.elseBody.stmts.isEmpty) {
      stmts = oldStmts :+
        st"""if ($cond) {
        |  ${(stmts, "\n")}
        |}"""
    } else {
      val tstmts = stmts
      stmts = ISZ()
      for (stmt <- stmt.thenBody.stmts) {
        transpileStmt(stmt)
      }
      stmts = oldStmts :+
        st"""if ($cond) {
        |  ${(tstmts, "\n")}
        |} else {
        |  ${(stmts, "\n")}
        |}"""
    }
  }

  def transpileMatch(stmt: AST.Stmt.Match): Unit = {
    halt("TODO") // TODO
  }

  def transpileStmt(stmt: AST.Stmt): Unit = {

    def transpileLoc(stmt: AST.Stmt): Unit = {
      stmts = stmts :+ empty
      stmt.posOpt match {
        case Some(pos) =>
          if (config.lineNumber) {
            stmts = stmts :+ st"sfUpdateLoc(${pos.beginLine});"
          } else {
            stmts = stmts :+ st"// L${pos.beginLine}"
          }
        case _ =>
          if (config.lineNumber) {
            stmts = stmts :+ st"sfUpdateLoc(0);"
          } else {
            stmts = stmts :+ st"// L?"
          }
      }
    }

    def transVar(stmt: AST.Stmt.Var, init: AST.Stmt.Expr): Unit = {
      val t: AST.Typed = stmt.tipeOpt match {
        case Some(tipe) => tipe.typedOpt.get
        case _ => init.typedOpt.get
      }
      typeKind(t) match {
        case TypeKind.Scalar =>
          stmts = stmts :+ st"${transpileType(t, F)} ${localName(stmt.id.value)} = ${transpileExp(init.exp)};"
        case _ => halt("TODO") // TODO
      }
    }

    def transVarComplex(stmt: AST.Stmt.Var): Unit = {
      halt("TODO") // TODO
    }

    def transAssign(stmt: AST.Stmt.Assign, rhs: AST.Stmt.Expr): Unit = {
      val t = stmt.lhs.typedOpt.get
      typeKind(t) match {
        case TypeKind.Scalar =>
          stmt.lhs match {
            case lhs: AST.Exp.Ident =>
              lhs.attr.resOpt.get match {
                case res: AST.ResolvedInfo.LocalVar =>
                  res.scope match {
                    case AST.ResolvedInfo.LocalVar.Scope.Closure => halt("TODO") // TODO
                    case _ => stmts = stmts :+ st"${localName(lhs.id.value)} = ${transpileExp(rhs.exp)};"
                  }
                case res: AST.ResolvedInfo.Var => halt("TODO") // TODO
                case _ => halt("Infeasible")
              }
            case lhs: AST.Exp.Select => halt("TODO") // TODO
            case lhs: AST.Exp.Invoke => halt("TODO") // TODO
            case _ => halt("Infeasible")
          }
        case _ => halt("TODO") // TODO
      }
    }

    def transAssignComplex(assign: AST.Stmt.Assign): Unit = {
      halt("TODO") // TODO
    }

    def transAssert(exp: AST.Exp.Invoke): Unit = {
      val kind: AST.ResolvedInfo.BuiltIn.Kind.Type = exp.attr.resOpt.get match {
        case AST.ResolvedInfo.BuiltIn(k) => k
        case _ => halt("Infeasible")
      }
      val cond = transpileExp(exp.args(0))
      if (kind == AST.ResolvedInfo.BuiltIn.Kind.Assert) {
        stmts = stmts :+ st"""if ($cond) sfAbort(sf, "Assertion failure");"""
      } else {
        assert(kind == AST.ResolvedInfo.BuiltIn.Kind.AssertMsg)
        val oldStmts = stmts
        stmts = ISZ()
        val s = transpileExp(exp.args(1))
        stmts = stmts :+
          st"""if ($cond) {
          |  ${(stmts, "\n")}
          |  sfAbort(sf, ($s)->value);
          |}"""
        stmts = oldStmts
      }
    }

    def transAssume(exp: AST.Exp.Invoke): Unit = {
      val kind: AST.ResolvedInfo.BuiltIn.Kind.Type = exp.attr.resOpt.get match {
        case AST.ResolvedInfo.BuiltIn(k) => k
        case _ => halt("Infeasible")
      }
      val cond = transpileExp(exp.args(0))
      if (kind == AST.ResolvedInfo.BuiltIn.Kind.Assume) {
        stmts = stmts :+ st"""if ($cond) sfAbort(sf, "Assumption does not hold");"""
      } else {
        assert(kind == AST.ResolvedInfo.BuiltIn.Kind.AssumeMsg)
        val oldStmts = stmts
        stmts = ISZ()
        val s = transpileExp(exp.args(1))
        stmts = stmts :+
          st"""if ($cond) {
          |  ${(stmts, "\n")}
          |  sfAbort(sf, ($s)->value);
          |}"""
        stmts = oldStmts
      }
    }

    def transCprint(exp: AST.Exp.Invoke): Unit = {
      val t = transpileExp(exp.args(0))
      for (i <- z"1" until exp.args.size) {
        transPrintH(t, exp.args(i))
      }
    }

    def transCprintln(exp: AST.Exp.Invoke): Unit = {
      val t = transpileExp(exp.args(0))
      val t2 = freshTempName()
      for (i <- z"1" until exp.args.size) {
        transPrintH(t2, exp.args(i))
      }
      stmts = stmts :+ st"cprintln($t2);"
      stmts = stmts :+ st"cflush($t2);"
    }

    def transEprint(exp: AST.Exp.Invoke): Unit = {
      for (i <- z"0" until exp.args.size) {
        transPrintH(falseLit, exp.args(i))
      }
    }

    def transEprintln(exp: AST.Exp.Invoke): Unit = {
      for (i <- z"0" until exp.args.size) {
        transPrintH(falseLit, exp.args(i))
      }
      stmts = stmts :+ st"cprintln($falseLit);"
      stmts = stmts :+ st"cflush($falseLit);"
    }

    def transPrint(exp: AST.Exp.Invoke): Unit = {
      for (i <- z"0" until exp.args.size) {
        transPrintH(trueLit, exp.args(i))
      }
    }

    def transPrintln(exp: AST.Exp.Invoke): Unit = {
      for (i <- z"0" until exp.args.size) {
        transPrintH(trueLit, exp.args(i))
      }
      stmts = stmts :+ st"cprintln($trueLit);"
      stmts = stmts :+ st"cflush($trueLit);"
    }

    def transHalt(exp: AST.Exp.Invoke): Unit = {
      val tmp = declString()
      transToString(tmp, exp.args(0))
      stmts = stmts :+ st"sfAbort(sf, $tmp->value);"
      stmts = stmts :+ abort
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

    def transpileWhile(stmt: AST.Stmt.While): Unit = {
      val cond = transpileExp(stmt.cond)
      val tmp: String = stmt.posOpt match {
        case Some(pos) => s"t_${pos.beginLine}_${pos.beginColumn}"
        case _ =>
          var h = stmt.hash
          if (h < 0) {
            h = h * h
          }
          s"t__$h"
      }
      stmts = stmts :+ st"B $tmp = $cond;"
      val oldStmts = stmts
      stmts = ISZ()
      for (stmt <- stmt.body.stmts) {
        transpileStmt(stmt)
      }
      transpileLoc(stmt)
      val cond2 = transpileExp(stmt.cond)
      stmts = stmts :+ st"$tmp = $cond2;"
      stmts = oldStmts :+
        st"""while($tmp) {
        |  ${(stmts, "\n")}
        |}"""
    }

    def transpileDoWhile(stmt: AST.Stmt.DoWhile): Unit = {
      val oldStmts = stmts
      stmts = ISZ()
      for (stmt <- stmt.body.stmts) {
        transpileStmt(stmt)
      }
      val cond = transpileExp(stmt.cond)
      stmts = oldStmts :+
        st"""{
        |  ${(stmts, "\n")}
        |} while($cond);"""
    }

    transpileLoc(stmt)

    stmt match {
      case stmt: AST.Stmt.Var =>
        stmt.initOpt.get match {
          case init: AST.Stmt.Expr => transVar(stmt, init)
          case _ => transVarComplex(stmt)
        }
      case stmt: AST.Stmt.Assign =>
        stmt.rhs match {
          case rhs: AST.Stmt.Expr => transAssign(stmt, rhs)
          case _ => transAssignComplex(stmt)
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
          case _ => halt("TODO") // TODO
        }
      case stmt: AST.Stmt.VarPattern => transpileVarPattern(stmt) // TODO
      case stmt: AST.Stmt.Block => transpileBlock(stmt)
      case stmt: AST.Stmt.If => transpileIf(stmt)
      case stmt: AST.Stmt.While => transpileWhile(stmt)
      case stmt: AST.Stmt.DoWhile => transpileDoWhile(stmt)
      case stmt: AST.Stmt.Match => transpileMatch(stmt)
      case _: AST.Stmt.Import => // skip
      case _: AST.Stmt.AbstractDatatype => // skip
      case _: AST.Stmt.Sig => // skip
      case _: AST.Stmt.Enum => // skip
      case _: AST.Stmt.Object => // skip
      case _: AST.Stmt.SpecMethod => // skip
      case _: AST.Stmt.SpecVar => // skip
      case _: AST.Stmt.TypeAlias => // skip
    }
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

  @pure def typeKind(t: AST.Typed): TypeKind.Type = {
    t match {
      case AST.Typed.b =>
      case AST.Typed.c =>
      case AST.Typed.z =>
      case AST.Typed.f32 =>
      case AST.Typed.f64 =>
      case AST.Typed.r =>
      case t: AST.Typed.Name if t.args.isEmpty =>
        th.typeMap.get(t.ids).get match {
          case _: TypeInfo.SubZ =>
          case _: TypeInfo.Enum =>
          case info: TypeInfo.AbstractDatatype =>
            return if (info.ast.isDatatype) if (info.ast.isRoot) TypeKind.ImmutableTrait else TypeKind.Immutable
            else if (info.ast.isRoot) TypeKind.MutableTrait
            else TypeKind.Mutable
          case info: TypeInfo.Sig =>
            return if (info.ast.isImmutable) TypeKind.ImmutableTrait else TypeKind.MutableTrait
          case _ => halt("Infeasible")
        }
      case t: AST.Typed.Tuple =>
        for (targ <- t.args) {
          typeKind(targ) match {
            case TypeKind.Mutable => return TypeKind.Mutable
            case TypeKind.MutableTrait => return TypeKind.Mutable
            case TypeKind.MS => return TypeKind.Mutable
            case _ =>
          }
        }
        return TypeKind.Immutable
      case _ => return TypeKind.Immutable
    }
    return TypeKind.Scalar
  }

  @pure def methodName(method: AST.ResolvedInfo.Method): String = {
    val tpe = method.tpeOpt.get
    var ids = method.owner :+ method.name
    if (config.fprintWidth != z"0" && (method.typeParams.nonEmpty || !method.isInObject)) {
      ids = ids :+ Fingerprint.string(tpe.string, config.fprintWidth)
    }
    if (!method.isInObject) {
      ids = "i" +: ids
    }
    return mangleName(ids)
  }

  def declString(): ST = {
    val tmp = freshTempName()
    val tmp2 = freshTempName()
    stmts = stmts :+ st"DeclNewString($tmp);"
    stmts = stmts :+ st"String $tmp2 = (String) &$tmp;"
    return tmp2
  }

  def freshTempName(): ST = {
    val r = st"t_$nextTempNum"
    nextTempNum = nextTempNum + 1
    return r
  }

  def escapeChar(posOpt: Option[Position], c: C): String = {
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
      reporter.error(posOpt, transKind, "Static C translation does not support Unicode string.")
      return "\\?"
    }
  }

}
