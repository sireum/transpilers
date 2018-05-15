// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.message._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver.QName
import org.sireum.transpilers.common.TypeSpecializer

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

  type SubstMap = HashMap[String, AST.Typed]

  @datatype class Config(
    projectName: String,
    lineNumber: B,
    fprintWidth: Z,
    defaultBitWidth: Z,
    maxStringSize: Z,
    maxArraySize: Z,
    customArraySizes: HashMap[AST.Typed, Z]
  )

  @datatype class Result(files: HashSMap[QName, ST])

  val transKind: String = "Static C Transpiler"
  val empty: ST = st""
  val trueLit: ST = st"T"
  val falseLit: ST = st"F"
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
  val noType: ST = st"TNONE"

  val builtInTypes: HashSet[AST.Typed] = HashSet ++ ISZ(
    AST.Typed.unit,
    AST.Typed.b,
    AST.Typed.c,
    AST.Typed.z,
    AST.Typed.f32,
    AST.Typed.f64,
    AST.Typed.r,
    AST.Typed.string
  )

  val iszStringType: AST.Typed.Name = AST.Typed.Name(AST.Typed.isName, ISZ(AST.Typed.z, AST.Typed.string))
  val mainType: AST.Typed.Fun = AST.Typed.Fun(F, F, ISZ(iszStringType), AST.Typed.unit)
}

import StaticTemplate._
import StaticTranspiler._

@record class StaticTranspiler(config: Config, ts: TypeSpecializer.Result, reporter: Reporter) {
  var compiledMap: HashMap[QName, Compiled] = HashMap.empty
  var typeNameMap: HashMap[AST.Typed, ST] = HashMap.empty
  var mangledTypeNameMap: HashMap[String, AST.Typed] = HashMap.empty

  var stmts: ISZ[ST] = ISZ()
  var nextTempNum: Z = 0

  def transpile(): Result = {

    var r = HashSMap.empty[QName, ST]
    var cFilenames: ISZ[String] = ISZ()

    def transEntryPoints(): Unit = {

      var i = 0
      for (ep <- ts.entryPoints) {
        ep match {
          case ep: TypeSpecializer.EntryPoint.Worksheet =>
            val (exeName, main) = transpileWorksheet(ep.program, i)
            val cFilename = s"$exeName.c"
            r = r + ISZ[String](cFilename) ~> main
            cFilenames = cFilenames :+ exeName
            i = i + 1
          case ep: TypeSpecializer.EntryPoint.Method =>
            val m = ts.typeHierarchy.nameMap.get(ep.name).get.asInstanceOf[Info.Method]
            val res = m.methodRes
            if (res.id == string"main" && res.tpeOpt.get == mainType) {
              val (exeName, main) = transpileMain(m, i)
              val cFilename = s"$exeName.c"
              r = r + ISZ[String](cFilename) ~> main
              cFilenames = cFilenames :+ exeName
              i = i + 1
            }
        }
      }
    }

    def work(): Unit = {
      for (ms <- ts.methods.values; m <- ms.elements) {
        transpileMethod(m)
      }
      for (m <- ts.traitMethods.elements) {
        transpileTraitMethod(m)
      }
    }

    def genFiles(): Unit = {

      val runtimeDir = string"runtime"
      val files = Runtime.staticFiles
      for (e <- files.entries) {
        val k = e._1
        if (k.size == z"1") {
          r = r + ISZ(runtimeDir, k(0)) ~> st"${e._2}"
        }
      }
      val ztypeFilename = string"ztype.h"
      val ztype: ST = config.defaultBitWidth match {
        case z"8" => st"${files.get(ISZ("8", ztypeFilename)).get}"
        case z"16" => st"${files.get(ISZ("16", ztypeFilename)).get}"
        case z"32" => st"${files.get(ISZ("32", ztypeFilename)).get}"
        case z"64" => st"${files.get(ISZ("64", ztypeFilename)).get}"
        case _ => halt("Infeasible")
      }
      r = r + ISZ(runtimeDir, ztypeFilename) ~> ztype

      val typeNames: ISZ[(String, ST)] = {
        var tn: ISZ[(String, ST)] = ISZ()
        for (e <- typeNameMap.entries) {
          if (!builtInTypes.contains(e._1) || e._1 == AST.Typed.string) {
            tn = tn :+ ((e._1.string, e._2))
          }
        }
        ops.ISZOps(tn).sortWith((p1, p2) => p1._1 <= p2._1)
      }

      val typeQNames = compiledMap.keys
      r = r + ISZ[String]("type-composite.h") ~> typeCompositeH(
        config.maxStringSize,
        maxElementSize(iszStringType),
        typeNames
      )
      r = r + ISZ[String]("types.h") ~> typesH(typeQNames, typeNames)
      r = r + ISZ[String]("types.c") ~> typesC()
      r = r + ISZ[String]("all.h") ~> allH(typeQNames)
      r = r ++ compiled(compiledMap)
      r = r + ISZ[String]("CMakeLists.txt") ~> cmake(config.projectName, cFilenames, r.keys.elements)
      r = r + ISZ[String]("typemap.properties") ~> typeManglingMap(
        for (e <- mangledTypeNameMap.entries) yield (e._1, e._2.string)
      )
    }

    genTypeNames()
    transEntryPoints()
    work()
    genFiles()

    return Result(r)
  }

  @pure def maxElementSize(t: AST.Typed): Z = {
    config.customArraySizes.get(t) match {
      case Some(n) => return n
      case _ => return config.maxArraySize
    }
  }

  def getCompiled(key: QName): Compiled = {
    compiledMap.get(key) match {
      case Some(r) => return r
      case _ => return Compiled(ISZ(), ISZ(), ISZ())
    }
  }

  def genTypeNames(): Unit = {
    @pure def typeFilename(name: QName, t: AST.Typed): Option[QName] = {
      val tname: QName = t match {
        case t: AST.Typed.Name => t.ids
        case t: AST.Typed.Tuple => AST.Typed.sireumName :+ s"Tuple${t.args.size}"
        case t: AST.Typed.Fun => AST.Typed.sireumName :+ s"Fun${t.args.size}"
        case t: AST.Typed.Enum => t.name
        case _ => halt("Infeasible")
      }
      return if (tname == name) None() else Some(tname)
    }

    @pure def includes(name: QName, ts: ISZ[AST.Typed]): ISZ[ST] = {
      var r = ISZ[ST]()
      for (t <- ts) {
        if (!builtInTypes.contains(t)) {
          typeFilename(name, t) match {
            case Some(n) => r = r :+ st"#include <${typeHeaderFilename(filenameOf(n))}>"
            case _ =>
          }
        }
      }
      return r
    }

    def genArray(t: AST.Typed.Name, mangledName: ST): Unit = {
      val key = t.ids
      val it = t.args(0)
      val et = t.args(1)
      val indexType = genType(it)
      val elementType = genType(et)
      val minIndex: Z = if (it == AST.Typed.z) {
        0
      } else {
        ts.typeHierarchy.typeMap.get(it.asInstanceOf[AST.Typed.Name].ids).get match {
          case info: TypeInfo.SubZ => if (info.ast.isZeroIndex) 0 else info.ast.min
          case _ => halt("Infeasible")
        }
      }
      val value = getCompiled(key)
      val newValue = array(
        value,
        includes(key, ISZ(it, et)),
        t.string,
        t.ids == AST.Typed.isName,
        mangledName,
        indexType,
        minIndex,
        typeKind(it) == TypeKind.Scalar,
        elementType,
        transpileType(et, T),
        maxElementSize(et)
      )
      compiledMap = compiledMap + key ~> newValue
    }
    def genClass(t: AST.Typed.Name, mangledName: ST): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genTrait(t: AST.Typed.Name, mangledName: ST): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genSubZ(t: AST.Typed.Name, mangledName: ST): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genTypedTuple(t: AST.Typed.Tuple, mangledName: ST): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genTypedFun(t: AST.Typed.Fun, mangledName: ST): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genTypedEnum(t: AST.Typed.Enum, mangledName: ST): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genType(t: AST.Typed): ST = {
      def fprint: String = {
        return Fingerprint.string(t.string, config.fprintWidth)
      }
      typeNameMap.get(t) match {
        case Some(tr) => return tr
        case _ =>
      }
      val (r, fprinted): (ST, B) = t match {
        case t: AST.Typed.Name =>
          val p: (ST, B) = if (t.args.isEmpty) (mangleName(t.ids), F) else (st"${mangleName(t.ids)}_$fprint", T)
          if (!builtInTypes.contains(t)) {
            typeKind(t) match {
              case TypeKind.Scalar => genSubZ(t, p._1)
              case TypeKind.Immutable => genClass(t, p._1)
              case TypeKind.ImmutableTrait => genTrait(t, p._1)
              case TypeKind.Mutable => genClass(t, p._1)
              case TypeKind.MutableTrait => genTrait(t, p._1)
              case TypeKind.IS => genArray(t, p._1)
              case TypeKind.MS => genArray(t, p._1)
            }
          }
          p
        case t: AST.Typed.Tuple =>
          val p = (st"Tuple${t.args.size}_$fprint", T)
          genTypedTuple(t, p._1)
          p
        case t: AST.Typed.Fun =>
          val p = (st"Fun${t.args.size}_$fprint", T)
          genTypedFun(t, p._1)
          p
        case t: AST.Typed.Enum =>
          val p = (mangleName(t.name), F)
          genTypedEnum(t, p._1)
          p
        case _ => halt("Infeasible: $t")
      }
      if (fprinted) {
        val tString = r.render
        mangledTypeNameMap.get(tString) match {
          case Some(t2) =>
            if (t != t2) {
              reporter.error(
                None(),
                transKind,
                st"Type name mangling collision is detected for '$t2' and '$t}' as '$tString' as (please increase fingerprint width).".render
              )
            }
          case _ => mangledTypeNameMap = mangledTypeNameMap + tString ~> t
        }
      }
      typeNameMap = typeNameMap + t ~> r
      return r
    }

    genType(AST.Typed.string)
    genType(iszStringType)

    for (nts <- ts.nameTypes.values; nt <- nts.elements) {
      ts.typeHierarchy.typeMap.get(nt.tpe.ids).get match {
        case _: TypeInfo.AbstractDatatype => genType(nt.tpe)
        case _: TypeInfo.Sig => genType(nt.tpe)
        case _ =>
      }
    }
    for (t <- ts.otherTypes.elements) {
      genType(t)
    }
  }

  def transpileMain(m: Info.Method, i: Z): (String, ST) = {
    val fileUriOpt: Option[String] = m.ast.posOpt match {
      case Some(pos) => pos.uriOpt
      case _ => None()
    }
    val fname = filename(fileUriOpt)
    val exeName = removeExt(fname)
    return (
      if (i == z"0") exeName else if (exeName == string"main") s"main$i" else exeName,
      main(fname, m.owner, m.ast.sig.id.value)
    )
  }

  def transpileWorksheet(program: AST.TopUnit.Program, i: Z): (String, ST) = {
    val fname = filename(program.fileUriOpt)
    val exeName = removeExt(fname)
    stmts = ISZ()
    nextTempNum = 0
    assert(program.packageName.ids.isEmpty)
    for (stmt <- program.body.stmts) {
      transpileStmt(stmt)
    }
    return (if (i == z"0") exeName else if (exeName == string"main") s"main$i" else exeName, worksheet(fname, stmts))
  }

  def transpileTraitMethod(method: TypeSpecializer.SMethod): Unit = {
    halt("TODO") // TODO
  }

  def transpileMethod(method: TypeSpecializer.Method): Unit = {
    @pure def compName(name: QName): QName = {
      var infoOpt = ts.typeHierarchy.nameMap.get(name)
      var currName = name
      while (infoOpt.isEmpty) {
        currName = ops.ISZOps(name).dropRight(1)
        infoOpt = ts.typeHierarchy.nameMap.get(currName)
      }
      return currName
    }
    val oldNextTempNum = nextTempNum
    val oldStmts = stmts

    nextTempNum = 0
    stmts = ISZ()

    val info = method.info.methodRes
    val header = methodHeader(method)
    for (stmt <- method.info.ast.bodyOpt.get.stmts) {
      transpileStmt(stmt)
    }
    val impl =
      st"""$header {
      |  DeclNewStackFrame(caller, "${filenameOfPosOpt(method.info.ast.posOpt)}", "${dotName(info.owner)}", "${info.id}", 0);
      |  ${(stmts, "\n")}
      |}"""

    val key: QName = info.owner.size match {
      case z"0" => info.owner
      case n if n >= 3 && ops.ISZOps(info.owner).take(3) == AST.Typed.sireumName =>
        sireumDir +: ops.ISZOps(compName(info.owner)).drop(3)
      case _ => compName(info.owner)
    }

    val value = getCompiled(key)
    compiledMap = compiledMap + key ~> value(header = value.header :+ header, impl = value.impl :+ impl)

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
      val u8is = conversions.String.toU8is(exp.value)
      val value = MSZ.create[String](u8is.size, "")
      val posOpt = exp.posOpt
      for (i <- u8is.indices) {
        value(i) = escapeChar(posOpt, conversions.U32.toC(conversions.U8.toU32(u8is(i))))
      }
      return st"""string("${(value, "")}")"""
    }

    @pure def transSubZLit(exp: AST.Exp.StringInterpolate): ST = {
      val tname = typeName(exp.attr.typedOpt)
      val info: TypeInfo.SubZ = ts.typeHierarchy.typeMap.get(tname).get.asInstanceOf[TypeInfo.SubZ]
      val n = Z(exp.lits(0).value).get
      checkBitWidth(n, info.ast.bitWidth)
      return st"${(dotName(tname), "")}_C($n)"
    }

    def transIdent(exp: AST.Exp.Ident): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.LocalVar =>
          res.scope match {
            case AST.ResolvedInfo.LocalVar.Scope.Closure => halt("TODO") // TODO
            case _ => return localName(exp.id.value)
          }
        case res: AST.ResolvedInfo.Var =>
          if (res.owner == AST.Typed.sireumName && (res.id == string"T" || res.id == string"F")) {
            return if (res.id == string"T") trueLit else falseLit
          } else {
            halt(s"TODO: $res") // TODO
          }
        case _ => halt("Infeasible")
      }
    }

    def transBinary(exp: AST.Exp.Binary): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.BuiltIn =>
          val tname = typeName(exp.left.typedOpt)
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
          ts.typeHierarchy.typeMap.get(t.ids) match {
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
      case exp: AST.Exp.Ident => val r = transIdent(exp); return r
      case exp: AST.Exp.Binary => val r = transBinary(exp); return r
      case exp: AST.Exp.Unary => val r = transUnary(exp); return r
      case _ => halt(s"TODO: $exp") // TODO
    }
  }

  def transToString(s: ST, exp: AST.Exp): Unit = {
    val tmp = transpileExp(exp)
    exp.typedOpt.get match {
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.isName || t.ids == AST.Typed.msName) {
          stmts = stmts :+ st"A_string_${Fingerprint.string(t.args(0).string, config.fprintWidth)}($s, sf, $tmp);"
        } else if (t.args.isEmpty) {
          stmts = stmts :+ st"${mangleName(t.ids)}_string($tmp, $s);"
        } else {
          stmts = stmts :+ st"${mangleName(t.ids)}_string_${Fingerprint.string(t.args.string, config.fprintWidth)}($s, sf, $tmp);"
        }
      case t: AST.Typed.Tuple =>
        stmts = stmts :+ st"Tuple${t.args.size}_string_${Fingerprint.string(t.args.string, config.fprintWidth)}($s, sf, $tmp);"
      case t: AST.Typed.Enum =>
        stmts = stmts :+ st"${mangleName(t.name)}_string($s, sf, $tmp);"
      case t: AST.Typed.Fun =>
        stmts = stmts :+ st"Fun_string_${Fingerprint.string(t.string, config.fprintWidth)}($s, sf, $tmp);"
      case _ => halt("Infeasible")
    }
  }

  def transPrintH(isOut: ST, exp: AST.Exp): Unit = {
    // TODO: Gen print on demand
    val tmp = transpileExp(exp)
    exp.typedOpt.get match {
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.isName || t.ids == AST.Typed.msName) {
          stmts = stmts :+ st"A_cprint_${Fingerprint.string(t.args(0).string, config.fprintWidth)}($tmp, $isOut);"
        } else if (t.args.isEmpty) {
          stmts = stmts :+ st"${mangleName(t.ids)}_cprint($tmp, $isOut);"
        } else {
          stmts = stmts :+ st"${mangleName(t.ids)}_cprint_${Fingerprint.string(t.args.string, config.fprintWidth)}($tmp, $isOut);"
        }
      case t: AST.Typed.Tuple =>
        stmts = stmts :+ st"Tuple${t.args.size}_cprint_${Fingerprint.string(t.args.string, config.fprintWidth)}($tmp, $isOut);"
      case t: AST.Typed.Enum =>
        stmts = stmts :+ st"${mangleName(t.name)}_cprint($tmp, $isOut);"
      case t: AST.Typed.Fun =>
        stmts = stmts :+ st"Fun_cprint_${Fingerprint.string(t.string, config.fprintWidth)}($tmp, $isOut);"
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
      transpileLoc(stmt)
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
      transpileLoc(stmt)
      halt("TODO") // TODO
    }

    def transAssign(stmt: AST.Stmt.Assign, rhs: AST.Stmt.Expr): Unit = {
      transpileLoc(stmt)
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
      transpileLoc(stmt)
      halt("TODO") // TODO
    }

    def transAssert(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      val kind: AST.ResolvedInfo.BuiltIn.Kind.Type = exp.attr.resOpt.get match {
        case AST.ResolvedInfo.BuiltIn(k) => k
        case _ => halt("Infeasible")
      }
      val cond = transpileExp(exp.args(0))
      if (kind == AST.ResolvedInfo.BuiltIn.Kind.Assert) {
        stmts = stmts :+ st"""if (!($cond)) { sfAbort("Assertion failure"); }"""
      } else {
        assert(kind == AST.ResolvedInfo.BuiltIn.Kind.AssertMsg)
        val oldStmts = stmts
        stmts = ISZ()
        val s = transpileExp(exp.args(1))
        stmts = stmts :+
          st"""if (!($cond)) {
          |  ${(stmts, "\n")}
          |  sfAbort(($s)->value);
          |}"""
        stmts = oldStmts
      }
    }

    def transAssume(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      val kind: AST.ResolvedInfo.BuiltIn.Kind.Type = exp.attr.resOpt.get match {
        case AST.ResolvedInfo.BuiltIn(k) => k
        case _ => halt("Infeasible")
      }
      val cond = transpileExp(exp.args(0))
      if (kind == AST.ResolvedInfo.BuiltIn.Kind.Assume) {
        stmts = stmts :+ st"""if (!($cond)) { sfAbort("Assumption does not hold"); }"""
      } else {
        assert(kind == AST.ResolvedInfo.BuiltIn.Kind.AssumeMsg)
        val oldStmts = stmts
        stmts = ISZ()
        val s = transpileExp(exp.args(1))
        stmts = stmts :+
          st"""if (!($cond)) {
          |  ${(stmts, "\n")}
          |  sfAbort(($s)->value);
          |}"""
        stmts = oldStmts
      }
    }

    def transCprint(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      val t = transpileExp(exp.args(0))
      for (i <- z"1" until exp.args.size) {
        transPrintH(t, exp.args(i))
      }
    }

    def transCprintln(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      val t = transpileExp(exp.args(0))
      val t2 = freshTempName()
      for (i <- z"1" until exp.args.size) {
        transPrintH(t2, exp.args(i))
      }
      stmts = stmts :+ st"cprintln($t2);"
      stmts = stmts :+ st"cflush($t2);"
    }

    def transEprint(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      for (i <- z"0" until exp.args.size) {
        transPrintH(falseLit, exp.args(i))
      }
    }

    def transEprintln(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      for (i <- z"0" until exp.args.size) {
        transPrintH(falseLit, exp.args(i))
      }
      stmts = stmts :+ st"cprintln($falseLit);"
      stmts = stmts :+ st"cflush($falseLit);"
    }

    def transPrint(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      for (i <- z"0" until exp.args.size) {
        transPrintH(trueLit, exp.args(i))
      }
    }

    def transPrintln(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      for (i <- z"0" until exp.args.size) {
        transPrintH(trueLit, exp.args(i))
      }
      stmts = stmts :+ st"cprintln($trueLit);"
      stmts = stmts :+ st"cflush($trueLit);"
    }

    def transHalt(exp: AST.Exp.Invoke): Unit = {
      transpileLoc(stmt)
      val tmp = declString()
      transToString(tmp, exp.args(0))
      stmts = stmts :+ st"sfAbort($tmp->value);"
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
      transpileLoc(stmt)
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
      transpileLoc(stmt)
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

  @pure def methodHeader(method: TypeSpecializer.Method): ST = {
    val res = method.info.methodRes
    val name = methodName(res)
    val tpe = res.tpeOpt.get
    val retType = transpileType(tpe.ret, F)

    val preParams: ST = method.receiverOpt match {
      case Some(receiver) => st"Stackframe caller, ${transpileType(receiver, T)} this"
      case _ => st"StackFrame caller"
    }
    val params: ST =
      if (res.paramNames.isEmpty) preParams
      else
        st"$preParams, ${(
          for (p <- ops.ISZOps(tpe.args).zip(res.paramNames))
            yield st"${transpileType(p._1, T)} ${localName(p._2)}",
          ", "
        )}"
    return st"$retType $name($params)"
  }

  @pure def transpileType(tpe: AST.Typed, isPtr: B): ST = {
    val t = typeNameMap.get(tpe).get
    if (isPtr) {
      return st"$t"
    } else {
      typeKind(tpe) match {
        case TypeKind.ImmutableTrait => return st"union $t"
        case TypeKind.MutableTrait => return st"union $t"
        case TypeKind.Scalar => return st"$t"
        case _ => return st"struct $t"
      }
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
      case AST.Typed.string => return TypeKind.Immutable
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.isName) {
          return TypeKind.IS
        } else if (t.ids == AST.Typed.msName) {
          return TypeKind.MS
        } else {
          ts.typeHierarchy.typeMap.get(t.ids).get match {
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

  @pure def methodName(method: AST.ResolvedInfo.Method): ST = {
    val tpe = method.tpeOpt.get
    var ids = method.owner :+ method.id
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
        case ' ' => return " "
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
            return s"\\x${ops.COps.hex2c(c >>> '\u0004')}${ops.COps.hex2c(c & '\u000F')}"
          }
      }
    } else {
      reporter.error(
        posOpt,
        transKind,
        "Static C translation does not support Unicode character literal (use String literal instead)."
      )
      return "\\?"
    }
  }

}
