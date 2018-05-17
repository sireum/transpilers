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
    'ImmutableTrait
    'MutableTrait
    'Immutable
    'Mutable
    'IS
    'MS
    'R
    'Scalar64
    'Scalar32
    'Enum
    'Scalar16
    'Scalar8
    'Scalar1
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
  val optionName: QName = AST.Typed.optionName
  val someName: QName = AST.Typed.optionName
  val noneName: QName = AST.Typed.optionName
  val eitherName: QName = AST.Typed.sireumName :+ "Either"
  val moptionName: QName = AST.Typed.sireumName :+ "MOption"
  val meitherName: QName = AST.Typed.sireumName :+ "MEither"
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
        var tn: ISZ[(String, ST)] = ISZ((dotName(AST.Typed.stringName), st"${mangleName(AST.Typed.stringName)}"))
        for (e <- typeNameMap.entries) {
          if (!builtInTypes.contains(e._1) && !isScalar(typeKind(e._1))) {
            tn = tn :+ ((e._1.string, e._2))
          }
        }
        ops.ISZOps(tn).sortWith((p1, p2) => p1._1 <= p2._1)
      }

      val typeQNames = compiledMap.keys
      r = r + ISZ[String]("type-composite.h") ~> typeCompositeH(
        config.maxStringSize,
        minIndexMaxElementSize(AST.Typed.z, AST.Typed.string)._2,
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

  @pure def minIndexMaxElementSize(indexType: AST.Typed, elementType: AST.Typed): (Z, Z) = {
    elementType match {
      case elementType: AST.Typed.Name if elementType.args.isEmpty =>
        ts.typeHierarchy.typeMap.get(elementType.ids).get match {
          case info: TypeInfo.Enum => return (z"0", info.elements.size)
          case _ =>
        }
      case _ =>
    }
    val size: Z = config.customArraySizes.get(elementType) match {
      case Some(n) => n
      case _ => config.maxArraySize
    }
    if (indexType == AST.Typed.z) {
      return (z"0", size)
    }
    val ast =
      ts.typeHierarchy.typeMap.get(indexType.asInstanceOf[AST.Typed.Name].ids).get.asInstanceOf[TypeInfo.SubZ].ast
    if (ast.isZeroIndex) {
      if (ast.hasMin && ast.hasMax) {
        val d = ast.max + 1
        return (z"0", if (d < size) d else size)
      } else {
        return (z"0", size)
      }
    } else {
      if (ast.hasMax) {
        val d = ast.max + -ast.min + 1
        return (ast.min, if (d < size) d else size)
      } else {
        return (ast.min, size)
      }
    }
  }

  def getCompiled(key: QName): Compiled = {
    compiledMap.get(key) match {
      case Some(r) => return r
      case _ => return Compiled(ISZ(), ISZ(), ISZ())
    }
  }

  @pure def fingerprint(t: AST.Typed): (ST, B) = {
    def fprint: String = {
      return Fingerprint.string(t.string, config.fprintWidth)
    }
    t match {
      case t: AST.Typed.Name =>
        ts.typeHierarchy.typeMap.get(t.ids).get match {
          case _: TypeInfo.Enum => return (mangleName(ops.ISZOps(t.ids).dropRight(1)), F)
          case _ => return if (t.args.isEmpty) (mangleName(t.ids), F) else (st"${mangleName(t.ids)}_$fprint", T)
        }
      case t: AST.Typed.Tuple => return (st"Tuple${t.args.size}_$fprint", T)
      case t: AST.Typed.Fun => return (st"Fun${t.args.size}_$fprint", T)
      case _ => halt(s"Infeasible: $t")
    }
  }

  def genTypeNames(): Unit = {
    @pure def typeFilename(name: QName, t: AST.Typed): Option[QName] = {
      val tname: QName = t match {
        case t: AST.Typed.Name => t.ids
        case t: AST.Typed.Tuple => AST.Typed.sireumName :+ s"Tuple${t.args.size}"
        case t: AST.Typed.Fun => AST.Typed.sireumName :+ s"Fun${t.args.size}"
        case _ => halt("Infeasible")
      }
      return if (tname == name) None() else if (tname.size == z"1") None() else Some(tname)
    }

    @pure def includes(name: QName, ts: ISZ[AST.Typed]): ISZ[ST] = {
      var r = ISZ[ST]()
      for (t <- ts) {
        if (!builtInTypes.contains(t) && !isScalar(typeKind(t))) {
          typeFilename(name, t) match {
            case Some(n) => r = r :+ st"#include <${typeHeaderFilename(filenameOf(n))}>"
            case _ =>
          }
        }
      }
      return r
    }

    def genArray(t: AST.Typed.Name): Unit = {
      val key = t.ids
      val it = t.args(0)
      val et = t.args(1)
      val indexType = genType(it)
      val elementType = genType(et)
      val value = getCompiled(key)
      val (minIndex, maxElementSize) = minIndexMaxElementSize(it, et)
      val newValue = array(
        value,
        includes(key, ISZ(it, et)),
        t.string,
        t.ids == AST.Typed.isName,
        fingerprint(t)._1,
        indexType,
        minIndex,
        isScalar(typeKind(it)),
        elementType,
        transpileType(et, T),
        maxElementSize
      )
      compiledMap = compiledMap + key ~> newValue
    }
    def genEnum(t: AST.Typed.Name): ST = {
      val name = ops.ISZOps(t.ids).dropRight(1)
      val mangledName = mangleName(name)
      typeNameMap = typeNameMap + t ~> mangledName
      val info = ts.typeHierarchy.nameMap.get(name).get.asInstanceOf[Info.Enum]
      val elements = info.elements.keys
      val elementType = info.elementTypedOpt.get
      val optionElementType = AST.Typed.Name(optionName, ISZ(elementType))
      val optElementTypeOpt: Option[(ST, ST, ST)] = ts.nameTypes.get(optionName) match {
        case Some(s) if s.contains(TypeSpecializer.NamedType(optionElementType, Map.empty, Map.empty)) =>
          val someElementType = AST.Typed.Name(someName, ISZ(elementType))
          val noneElementType = AST.Typed.Name(noneName, ISZ(elementType))
          genType(someElementType)
          genType(noneElementType)
          Some((fingerprint(optionElementType)._1, fingerprint(someElementType)._1, fingerprint(noneElementType)._1))
        case _ => None()
      }
      val iszElementType = AST.Typed.Name(AST.Typed.isName, ISZ(AST.Typed.z, elementType))
      val iszElementTypeOpt: Option[ST] = ts.nameTypes.get(AST.Typed.isName) match {
        case Some(s) if s.contains(TypeSpecializer.NamedType(iszElementType, Map.empty, Map.empty)) =>
          Some(fingerprint(iszElementType)._1)
        case _ => None()
      }
      val value = getCompiled(name)
      val newValue =
        enum(value, filenameOfPosOpt(info.posOpt, ""), name, elements, optElementTypeOpt, iszElementTypeOpt)
      compiledMap = compiledMap + name ~> newValue
      return mangledName
    }
    def genSubZ(t: AST.Typed.Name): Unit = {
      val name = t.ids
      val mangledName = mangleName(name)
      typeNameMap = typeNameMap + t ~> mangledName
      val info = ts.typeHierarchy.typeMap.get(name).get.asInstanceOf[TypeInfo.SubZ]
      val optionType = AST.Typed.Name(optionName, ISZ(t))
      val optionTypeOpt: Option[(ST, ST, ST)] = ts.nameTypes.get(optionName) match {
        case Some(s) if s.contains(TypeSpecializer.NamedType(optionType, Map.empty, Map.empty)) =>
          val someType = AST.Typed.Name(someName, ISZ(t))
          val noneType = AST.Typed.Name(noneName, ISZ(t))
          genType(someType)
          genType(noneType)
          Some((fingerprint(optionType)._1, fingerprint(someType)._1, fingerprint(noneType)._1))
        case _ => None()
      }
      val value = getCompiled(name)
      val ast = info.ast
      val newValue = subz(
        value,
        filenameOfPosOpt(info.posOpt, ""),
        name,
        ast.bitWidth,
        ast.isBitVector,
        !ast.isSigned,
        if (ast.hasMin) Some(ast.min) else None(),
        if (ast.hasMax) Some(ast.max) else None(),
        optionTypeOpt
      )
      compiledMap = compiledMap + name ~> newValue
    }
    def genClass(t: AST.Typed.Name): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genTrait(t: AST.Typed.Name): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genTuple(t: AST.Typed.Tuple): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genFun(t: AST.Typed.Fun): Unit = {
      halt(s"TODO: $t") // TODO
    }
    def genType(t: AST.Typed): ST = {
      typeNameMap.get(t) match {
        case Some(tr) => return tr
        case _ =>
      }
      t match {
        case t: AST.Typed.Name =>
          if (!builtInTypes.contains(t)) {
            typeKind(t) match {
              case TypeKind.Immutable => genClass(t)
              case TypeKind.ImmutableTrait => genTrait(t)
              case TypeKind.Mutable => genClass(t)
              case TypeKind.MutableTrait => genTrait(t)
              case TypeKind.IS => genArray(t)
              case TypeKind.MS => genArray(t)
              case TypeKind.Enum => val r = genEnum(t); return r
              case _ => genSubZ(t)
            }
          }
        case t: AST.Typed.Tuple =>
          genTuple(t)
        case t: AST.Typed.Fun =>
          genFun(t)
        case _ => halt("Infeasible: $t")
      }
      val p = fingerprint(t)
      if (p._2) {
        val tString = p._1.render
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
      typeNameMap = typeNameMap + t ~> p._1
      return p._1
    }

    genType(AST.Typed.string)

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
    val fname = filename(fileUriOpt, "main")
    val exeName = removeExt(fname)
    return (
      if (i == z"0") exeName else if (exeName == string"main") s"main$i" else exeName,
      main(fname, m.owner, m.ast.sig.id.value)
    )
  }

  def transpileWorksheet(program: AST.TopUnit.Program, i: Z): (String, ST) = {
    val fname = filename(program.fileUriOpt, "main")
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
      |  DeclNewStackFrame(caller, "${filenameOfPosOpt(method.info.ast.posOpt, "")}", "${dotName(info.owner)}", "${info.id}", 0);
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
      val tname = typeName(expType(exp))
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
          val tname = typeName(expType(exp.left))
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
            val tname = typeName(expType(exp))
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
      expType(exp) match {
        case t: AST.Typed.Name if t.args.isEmpty =>
          ts.typeHierarchy.typeMap.get(t.ids) match {
            case Some(_: TypeInfo.SubZ) => return T
            case _ => return F
          }
        case _ => return F
      }
    }

    def transSelect(select: AST.Exp.Select): ST = {
      select.attr.resOpt.get match {
        case res: AST.ResolvedInfo.EnumElement => return elementName(res.owner, res.name)
        case res: AST.ResolvedInfo.BuiltIn =>
          res.kind match {
            case AST.ResolvedInfo.BuiltIn.Kind.EnumElements =>
              val owner = expType(select.receiverOpt.get).asInstanceOf[AST.Typed.Enum].name
              val iszType = fingerprint(expType(select))._1
              val temp = freshTempName()
              stmts = stmts :+ st"DeclNew$iszType($temp);"
              stmts = stmts :+ st"${mangleName(owner)}_elements(&$temp);"
              return st"(&$temp)"
            case AST.ResolvedInfo.BuiltIn.Kind.EnumNumOfElements =>
              val owner = expType(select.receiverOpt.get).asInstanceOf[AST.Typed.Enum].name
              val temp = freshTempName()
              stmts = stmts :+ st"Z $temp = ${mangleName(owner)}_numOfElements();"
              return temp
            case _ => halt(s"TODO: ${res.kind}") // TODO
          }
        case res => halt(s"TODO: $res") // TODO
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
      case exp: AST.Exp.Select => val r = transSelect(exp); return r
      case _ => halt(s"TODO: $exp") // TODO
    }
  }

  def transToString(s: ST, exp: AST.Exp): Unit = {
    val tmp = transpileExp(exp)
    val mangledName = fingerprint(expType(exp))._1
    stmts = stmts :+ st"${mangledName}_string($s, sf, $tmp);"
  }

  def transPrintH(isOut: ST, exp: AST.Exp): Unit = {
    val tmp = transpileExp(exp)
    val mangledName = fingerprint(expType(exp))._1
    stmts = stmts :+ st"${mangledName}_cprint($tmp, $isOut);"
  }

  def expType(exp: AST.Exp): AST.Typed = {
    exp.typedOpt.get match {
      case t: AST.Typed.Method if t.tpe.isByName => return t.tpe.ret
      case t => return t
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
      if (isScalar(typeKind(t))) {
        stmts = stmts :+ st"${transpileType(t, F)} ${localName(stmt.id.value)} = ${transpileExp(init.exp)};"
      } else {
        halt("TODO") // TODO
      }
    }

    def transVarComplex(stmt: AST.Stmt.Var): Unit = {
      transpileLoc(stmt)
      halt("TODO") // TODO
    }

    def transAssign(stmt: AST.Stmt.Assign, rhs: AST.Stmt.Expr): Unit = {
      transpileLoc(stmt)
      val t = expType(stmt.lhs)
      if (isScalar(typeKind(t))) {
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
      } else {
        halt("TODO") // TODO
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
        case kind => return if (isScalar(kind)) st"$t" else st"struct $t"
      }
    }
  }

  @pure def isScalar(kind: TypeKind.Type): B = {
    kind match {
      case TypeKind.Scalar1 =>
      case TypeKind.Scalar8 =>
      case TypeKind.Scalar16 =>
      case TypeKind.Scalar32 =>
      case TypeKind.Scalar64 =>
      case TypeKind.R =>
      case TypeKind.Enum =>
      case _ => return F
    }
    return T
  }

  @memoize def typeKind(t: AST.Typed): TypeKind.Type = {
    @pure def bitWidthKindNumber(size: Z): TypeKind.Type = {
      if (size < u8Max) {
        return TypeKind.Scalar8
      } else if (size < u16Max) {
        return TypeKind.Scalar16
      } else if (size < u32Max) {
        return TypeKind.Scalar32
      } else if (size < u64Max) {
        return TypeKind.Scalar64
      } else {
        halt("Infeasible")
      }
    }
    @pure def bitWidthKind(n: Z): TypeKind.Type = {
      n match {
        case z"8" => return TypeKind.Scalar8
        case z"16" => return TypeKind.Scalar16
        case z"32" => return TypeKind.Scalar32
        case z"64" => return TypeKind.Scalar64
        case _ => halt("Infeasible")
      }
    }
    t match {
      case AST.Typed.b => return TypeKind.Scalar1
      case AST.Typed.c => return TypeKind.Scalar8
      case AST.Typed.z => bitWidthKind(config.defaultBitWidth)
      case AST.Typed.f32 => return TypeKind.Scalar32
      case AST.Typed.f64 => return TypeKind.Scalar64
      case AST.Typed.r => return TypeKind.R
      case AST.Typed.string => return TypeKind.Immutable
      case t: AST.Typed.Name =>
        if (t.ids == AST.Typed.isName) {
          return TypeKind.IS
        } else if (t.ids == AST.Typed.msName) {
          return TypeKind.MS
        } else {
          ts.typeHierarchy.typeMap.get(t.ids).get match {
            case info: TypeInfo.SubZ => return bitWidthKind(info.ast.bitWidth)
            case _: TypeInfo.Enum => return TypeKind.Enum
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
