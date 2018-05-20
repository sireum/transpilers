// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.message._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver.QName
import org.sireum.transpilers.common.TypeSpecializer

import StaticTemplate._

object StaticTranspiler {

  type SubstMap = HashMap[String, AST.Typed]

  @datatype class Config(
    projectName: String,
    lineNumber: B,
    fprintWidth: Z,
    defaultBitWidth: Z,
    maxStringSize: Z,
    maxArraySize: Z,
    customArraySizes: HashMap[AST.Typed, Z],
    extMethodTranspilerPlugins: ISZ[ExtMethodTranspilerPlugin]
  )

  @datatype class Result(files: HashSMap[QName, ST])

  @sig trait ExtMethodTranspilerPlugin {

    @pure def canCompile(method: TypeSpecializer.SMethod): B

    @pure def transpile(
      compiled: Compiled,
      typeSpecializer: TypeSpecializer.Result,
      method: TypeSpecializer.SMethod
    ): Compiled
  }

  @datatype class NumberConversionsExtMethodTranspilerPlugin extends ExtMethodTranspilerPlugin {

    @pure def canCompile(method: TypeSpecializer.SMethod): B = {
      if (method.owner.size < 4) {
        return F
      }
      if (ops.ISZOps(method.owner).take(conversionsPkg.size) == conversionsPkg && scalarConversionObjects.contains(
          method.owner(3)
        )) {
        return T
      }
      return F
    }

    @pure def transpile(
      compiled: Compiled,
      typeSpecializer: TypeSpecializer.Result,
      method: TypeSpecializer.SMethod
    ): Compiled = {
      val from = ops.ISZOps(method.owner).takeRight(1)
      val id = method.id
      val sops = ops.StringOps(id)
      if (id.size > 2 && sops.startsWith("to")) {
        val to = ops.StringOps(id).substring(2, id.size)
        return compiled(
          header = compiled.header :+
            st"""static inline $to ${mangleName(method.owner)}_to$to(StackFrame sf, $from n) {
            |  return ($to) n;
            |}"""
        )
      } else {
        halt("TODO") // TODO
      }
    }
  }

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
  val someName: QName = AST.Typed.sireumName :+ "Some"
  val noneName: QName = AST.Typed.sireumName :+ "None"
  val eitherName: QName = AST.Typed.sireumName :+ "Either"
  val moptionName: QName = AST.Typed.sireumName :+ "MOption"
  val meitherName: QName = AST.Typed.sireumName :+ "MEither"

  val sNameSet: HashSet[QName] = HashSet.empty ++ ISZ(
    AST.Typed.isName,
    AST.Typed.iszName,
    AST.Typed.msName,
    AST.Typed.mszName,
    AST.Typed.zsName
  )

  val conversionsPkg: ISZ[String] = AST.Typed.sireumName :+ "conversions"

  val scalarConversionObjects: HashSet[String] = HashSet ++ ISZ(
    "Z",
    "Z8",
    "Z16",
    "Z32",
    "Z64",
    "N",
    "N8",
    "N16",
    "N32",
    "N64",
    "S8",
    "S16",
    "S32",
    "S64",
    "U8",
    "U16",
    "U32",
    "U64"
  )
}

import StaticTranspiler._

@record class StaticTranspiler(config: Config, ts: TypeSpecializer.Result, reporter: Reporter) {
  var compiledMap: HashMap[QName, Compiled] = HashMap.empty
  var typeNameMap: HashMap[AST.Typed, ST] = HashMap.empty
  var mangledTypeNameMap: HashMap[String, AST.Typed] = HashMap.empty

  var currReceiverOpt: Option[AST.Typed.Name] = None()
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

    def transpileObjectVars(name: QName, vars: HashSSet[String]): Unit = {
      val oldCurrReceiverOpt = currReceiverOpt
      val oldStmts = stmts
      val oldNextTempNum = nextTempNum
      val value = getCompiled(name)
      val oInfo = ts.typeHierarchy.nameMap.get(name).get.asInstanceOf[Info.Object]
      var vs = ISZ[(TypeKind.Type, String, ST, ST, B)]()
      stmts = ISZ()
      val mangledName = mangleName(name)
      for (stmt <- oInfo.ast.stmts) {
        stmt match {
          case stmt: AST.Stmt.Var if vars.contains(stmt.id.value) =>
            val id = stmt.id.value
            val t = stmt.tipeOpt.get.typedOpt.get
            val kind = typeKind(t)
            vs = vs :+ ((kind, id, typeDecl(t), transpileType(t), !stmt.isVal))
            transpileAssignExp(
              stmt.initOpt.get,
              (rhs, rhsT) =>
                if (isScalar(kind)) st"_${mangledName}_$id = $rhs;"
                else st"Type_assign(&_${mangledName}_$id, $rhs, sizeof($rhsT));"
            )
          case _ =>
        }
      }
      val uri = filenameOfPosOpt(oInfo.ast.posOpt, "")
      val newValue = obj(value, uri, name, mangledName, vs, stmts)
      currReceiverOpt = oldCurrReceiverOpt
      stmts = oldStmts
      nextTempNum = oldNextTempNum
      compiledMap = compiledMap + name ~> newValue
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

    def work(): Unit = {
      genTypeNames()
      for (ms <- ts.methods.values; m <- ms.elements) {
        transpileMethod(m)
      }
      for (p <- ts.objectVars.entries) {
        transpileObjectVars(p._1, p._2)
      }
      for (m <- ts.traitMethods.elements) {
        transpileTraitMethod(m)
      }
      for (m <- ts.extMethods.elements if !sNameSet.contains(m.owner)) {
        var found = F
        val name = m.owner
        val value = getCompiled(name)
        for (p <- config.extMethodTranspilerPlugins) {
          if (p.canCompile(m)) {
            found = T
            val newValue = p.transpile(value, ts, m)
            compiledMap = compiledMap + name ~> newValue
          }
        }
        if (!found) {
          reporter.info(None(), transKind, st"Generated @ext method header for '${(m.owner :+ m.id, ".")}'.".render)
          val info = ts.typeHierarchy.nameMap.get(m.owner :+ m.id).get.asInstanceOf[Info.ExtMethod]
          if (info.ast.sig.typeParams.nonEmpty) {
            val mh = methodHeader(
              m.receiverOpt,
              T,
              info.owner,
              info.ast.sig.id.value,
              info.ast.sig.typeParams.isEmpty,
              m.tpe,
              info.ast.sig.params.map((p: AST.Param) => p.id.value)
            )
            compiledMap = compiledMap + name ~> value(header = value.header :+ st"$mh;")
          }
        }
      }
      transEntryPoints()
      genFiles()
    }

    work()

    return Result(r)
  }

  def genClassConstructor(nt: TypeSpecializer.NamedType): Unit = {
    val t = nt.tpe
    val name = t.ids
    val value = getCompiled(name)
    var types = ISZ[AST.Typed]()
    var cps = ISZ[(TypeKind.Type, String, ST, ST)]()
    for (cv <- nt.constructorVars.entries) {
      val (id, (_, ct)) = cv
      types = types :+ ct
      val tpe = fingerprint(ct)._1
      val kind = typeKind(ct)
      cps = cps :+ ((kind, fieldName(id).render, typeDecl(ct), tpe))
    }
    val oldNextTempNum = nextTempNum
    val oldStmts = stmts
    val oldReceiverOpts = currReceiverOpt
    nextTempNum = 0
    stmts = ISZ()

    // TODO: other stmts
    for (v <- nt.vars.entries) {
      val (id, (_, ct, init)) = v
      val kind = typeKind(ct)
      if (isScalar(kind)) {
        transpileAssignExp(init, (rhs, _) => st"this->$id = $rhs;")
      } else {
        transpileAssignExp(init, (rhs, rhsT) => st"Type_assign(&this->$id, $rhs, sizeof($rhsT));")
      }
    }

    val uri =
      filenameOfPosOpt(ts.typeHierarchy.typeMap.get(name).get.asInstanceOf[TypeInfo.AbstractDatatype].posOpt, "")
    val newValue = claszConstructor(value, uri, name, fingerprint(t)._1, cps, stmts)
    compiledMap = compiledMap + name ~> newValue

    nextTempNum = oldNextTempNum
    stmts = oldStmts
    currReceiverOpt = oldReceiverOpts
  }

  def transpileAssignExp(exp: AST.AssignExp, f: (ST, ST) => ST @pure): Unit = {
    exp match {
      case exp: AST.Stmt.Expr =>
        val rhs = transpileExp(exp.exp)
        stmts = stmts :+ f(rhs, typeDecl(exp.typedOpt.get))
      case exp: AST.Stmt.Block => halt(s"TODO: $exp") // TODO
      case exp: AST.Stmt.If => halt(s"TODO: $exp") // TODO
      case exp: AST.Stmt.Match => halt(s"TODO: $exp") // TODO
      case exp: AST.Stmt.Return => halt(s"TODO: $exp")
    }
  }

  def classVars(
    nt: TypeSpecializer.NamedType
  ): (ISZ[AST.Typed], ISZ[(TypeKind.Type, String, ST, ST, B)], ISZ[(TypeKind.Type, String, ST, ST, B)]) = {
    var types = ISZ[AST.Typed]()
    var cps = ISZ[(TypeKind.Type, String, ST, ST, B)]()
    for (cv <- nt.constructorVars.entries) {
      val (id, (isVal, ct)) = cv
      types = types :+ ct
      val tpe = fingerprint(ct)._1
      val kind = typeKind(ct)
      cps = cps :+ ((kind, id, st"${typePrefix(kind)} $tpe", tpe, !isVal))
    }
    var vs = ISZ[(TypeKind.Type, String, ST, ST, B)]()
    for (v <- nt.vars.entries) {
      val (id, (isVal, ct, _)) = v
      types = types :+ ct
      val tpe = fingerprint(ct)._1
      val kind = typeKind(ct)
      vs = vs :+ ((kind, id, st"${typePrefix(kind)} $tpe", tpe, !isVal))
    }
    return (types, cps, vs)
  }

  @pure def minIndexMaxElementSize(indexType: AST.Typed, elementType: AST.Typed): (Z, Z) = {
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

  @pure def fprint(t: AST.Typed): ST = {
    val width = config.fprintWidth
    val max: Z = if (0 < width && width <= 64) width else 64
    val bytes = ops.ISZOps(crypto.SHA3.sum512(conversions.String.toU8is(t.string))).take(max)
    var cs = ISZ[C]()
    for (b <- bytes) {
      val c = conversions.U32.toC(conversions.U8.toU32(b))
      cs = cs :+ ops.COps.hex2c((c >>> '\u0004') & '\u000F')
      cs = cs :+ ops.COps.hex2c(c & '\u000F')
    }
    return st"$cs"
  }

  @pure def fingerprint(t: AST.Typed): (ST, B) = {
    t match {
      case t: AST.Typed.Name =>
        ts.typeHierarchy.typeMap.get(t.ids).get match {
          case _: TypeInfo.Enum => return (mangleName(ops.ISZOps(t.ids).dropRight(1)), F)
          case _ => return if (t.args.isEmpty) (mangleName(t.ids), F) else (st"${mangleName(t.ids)}_${fprint(t)}", T)
        }
      case t: AST.Typed.Tuple => return (st"Tuple${t.args.size}_${fprint(t)}", T)
      case t: AST.Typed.Fun => return (st"Fun${t.args.size}_${fprint(t)}", T)
      case _ => halt(s"Infeasible: $t")
    }
  }

  def genTypeNames(): Unit = {
    @pure def typeFilename(name: QName, t: AST.Typed): Option[QName] = {
      val tname: QName = t match {
        case t: AST.Typed.Name =>
          ts.typeHierarchy.typeMap.get(t.ids).get match {
            case _: TypeInfo.Enum => ops.ISZOps(t.ids).dropRight(1)
            case _ => t.ids
          }
        case t: AST.Typed.Tuple => AST.Typed.sireumName :+ s"Tuple${t.args.size}"
        case t: AST.Typed.Fun => AST.Typed.sireumName :+ s"Fun${t.args.size}"
        case _ => halt("Infeasible")
      }
      return if (tname == name) None() else if (tname.size == z"1") None() else Some(tname)
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

    def genArray(t: AST.Typed.Name): Unit = {
      val key = t.ids
      val it = t.args(0)
      val et = t.args(1)
      val etKind = typeKind(et)
      val indexType = genType(it)
      val elementType = typeDecl(et)
      genType(et)
      val value = getCompiled(key)
      val (minIndex, maxElementSize) = minIndexMaxElementSize(it, et)
      val otherType: AST.Typed.Name =
        if (key == AST.Typed.isName) AST.Typed.Name(AST.Typed.msName, ISZ(it, et))
        else AST.Typed.Name(AST.Typed.isName, ISZ(it, et))
      val otherTpeOpt: Option[ST] = ts.nameTypes.get(otherType.ids) match {
        case Some(s) if s.contains(TypeSpecializer.NamedType(otherType, Map.empty, Map.empty)) =>
          Some(fingerprint(otherType)._1)
        case _ => None()
      }
      val newValue = array(
        value,
        includes(key, ISZ(it, et)),
        t.string,
        t.ids == AST.Typed.isName,
        fingerprint(t)._1,
        otherTpeOpt,
        indexType,
        minIndex,
        isScalar(etKind),
        elementType,
        transpileType(et),
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
      val bw = ast.bitWidth
      val newValue = subz(
        value,
        filenameOfPosOpt(info.posOpt, ""),
        name,
        if (bw == z"0") config.defaultBitWidth else bw,
        ast.isBitVector,
        !ast.isSigned,
        if (ast.hasMin) Some(ast.min) else None(),
        if (ast.hasMax) Some(ast.max) else None(),
        optionTypeOpt
      )
      compiledMap = compiledMap + name ~> newValue
    }
    def genTuple(t: AST.Typed.Tuple): Unit = {
      val name = tupleName(t.args.size)
      val value = getCompiled(name)
      var paramTypes = ISZ[(TypeKind.Type, ST, ST)]()
      for (arg <- t.args) {
        genType(arg)
        val tPtr = fingerprint(arg)._1
        val kind = typeKind(arg)
        paramTypes = paramTypes :+ ((kind, typeDecl(arg), tPtr))
      }
      val newValue = tuple(value, t.string, includes(name, t.args), fingerprint(t)._1, paramTypes)
      compiledMap = compiledMap + name ~> newValue
    }
    def genClass(t: AST.Typed.Name): Unit = {
      val name = t.ids
      val value = getCompiled(name)
      for (nt <- ts.nameTypes.get(name).get.elements if nt.tpe == t) {
        var types = ISZ[AST.Typed]()
        var cps = ISZ[(TypeKind.Type, String, ST, ST, B)]()
        for (cv <- nt.constructorVars.entries) {
          val (id, (isVal, ct)) = cv
          types = types :+ ct
          val tpe = genType(ct)
          val kind = typeKind(ct)
          cps = cps :+ ((kind, fieldName(id).render, typeDecl(ct), tpe, !isVal))
        }
        var vs = ISZ[(TypeKind.Type, String, ST, ST, B)]()
        for (v <- nt.vars.entries) {
          val (id, (isVal, ct, _)) = v
          types = types :+ ct
          val tpe = genType(ct)
          val kind = typeKind(ct)
          vs = vs :+ ((kind, fieldName(id).render, typeDecl(ct), tpe, !isVal))
        }
        val uri =
          filenameOfPosOpt(ts.typeHierarchy.typeMap.get(name).get.asInstanceOf[TypeInfo.AbstractDatatype].posOpt, "")
        val newValue = clasz(value, uri, name, includes(name, types), t.string, fingerprint(t)._1, cps, vs)
        compiledMap = compiledMap + name ~> newValue
      }
    }
    def genTrait(t: AST.Typed.Name): Unit = {
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
        case _: TypeInfo.Enum => genType(nt.tpe)
        case _: TypeInfo.SubZ => genType(nt.tpe)
        case _ =>
      }
    }
    for (t <- ts.otherTypes.elements) {
      genType(t)
    }
    for (nts <- ts.nameTypes.values; nt <- nts.elements) {
      ts.typeHierarchy.typeMap.get(nt.tpe.ids).get match {
        case info: TypeInfo.AbstractDatatype if !info.ast.isRoot => genClassConstructor(nt)
        case _ =>
      }
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
    halt(s"TODO: $method") // TODO
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
    val oldCurrReceiverOpt = currReceiverOpt

    currReceiverOpt = method.receiverOpt
    nextTempNum = 0
    stmts = ISZ()

    val res = method.info.methodRes
    val header = methodHeaderRes(method.receiverOpt, res)
    for (stmt <- method.info.ast.bodyOpt.get.stmts) {
      transpileStmt(stmt)
    }
    val impl =
      st"""$header {
      |  DeclNewStackFrame(caller, "${filenameOfPosOpt(method.info.ast.posOpt, "")}", "${dotName(res.owner)}", "${res.id}", 0);
      |  ${(stmts, "\n")}
      |}"""

    val key: QName = res.owner.size match {
      case z"0" => res.owner
      case n if n >= 3 && ops.ISZOps(res.owner).take(3) == AST.Typed.sireumName =>
        sireumDir +: ops.ISZOps(compName(res.owner)).drop(3)
      case _ => compName(res.owner)
    }

    val value = getCompiled(key)
    compiledMap = compiledMap + key ~> value(header = value.header :+ st"$header;", impl = value.impl :+ impl)

    currReceiverOpt = oldCurrReceiverOpt
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

    def checkBitWidth(n: Z, bitWidth: Z, isUnsigned: B): Unit = {
      var ok = T
      val bw: Z = if (bitWidth == z"0") config.defaultBitWidth else bitWidth
      bw match {
        case z"8" =>
          if (isUnsigned) {
            ok = 0 <= n && n <= u8Max
          } else {
            ok = i8Min <= n && n <= i8Max
          }
        case z"16" =>
          if (isUnsigned) {
            ok = 0 <= n && n <= u16Max
          } else {
            ok = i16Min <= n && n <= i16Max
          }
        case z"32" =>
          if (isUnsigned) {
            ok = 0 <= n && n <= u32Max
          } else {
            ok = i32Min <= n && n <= i32Max
          }
        case z"64" =>
          if (isUnsigned) {
            ok = 0 <= n && n <= u64Max
          } else {
            ok = i64Min <= n && n <= i64Max
          }
        case _ => halt("Infeasible")
      }
      if (!ok) {
        reporter.error(exp.posOpt, transKind, s"Invalid ${config.defaultBitWidth}-bit Z literal '$n'.")
      }
    }

    def transLitZ(exp: AST.Exp.LitZ): ST = {
      val n = exp.value
      checkBitWidth(n, config.defaultBitWidth, F)
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
      checkBitWidth(n, info.ast.bitWidth, !info.ast.isSigned)
      return st"${mangleName(tname)}_C($n)"
    }

    def transIdent(exp: AST.Exp.Ident): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.LocalVar =>
          res.scope match {
            case AST.ResolvedInfo.LocalVar.Scope.Closure => halt(s"TODO: $exp") // TODO
            case _ => return localName(exp.id.value)
          }
        case res: AST.ResolvedInfo.Var =>
          if (res.owner == AST.Typed.sireumName && (res.id == string"T" || res.id == string"F")) {
            return if (res.id == string"T") trueLit else falseLit
          } else {
            if (res.isInObject) {
              val name = mangleName(res.owner :+ res.id)
              return st"$name(sf)"
            } else {
              val t = currReceiverOpt.get
              return st"${transpileType(t)}_${fieldName(res.id)}_(this)"
            }
          }
        case res: AST.ResolvedInfo.Method =>
          val t = res.tpeOpt.get.ret
          if (res.isInObject) {
            val r = transObjectMethodInvoke(t, res, ISZ())
            return r
          } else {
            val r = transInstanceMethodInvoke(currReceiverOpt.get, t, res, st"this", ISZ())
            return r
          }
        case res => halt(s"Infeasible: $res")
      }
    }

    def transBinary(exp: AST.Exp.Binary): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.BuiltIn =>
          res.kind match {
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryImply =>
              val left = transpileExp(exp.left)
              val right = transpileExp(exp.right)
              return st"(!($left) || $right)"
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryCondAnd =>
              val left = transpileExp(exp.left)
              val right = transpileExp(exp.right)
              return st"($left && $right)"
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryCondOr =>
              val left = transpileExp(exp.left)
              val right = transpileExp(exp.right)
              return st"($left || $right)"
            case AST.ResolvedInfo.BuiltIn.Kind.BinaryMapsTo =>
              val left = transpileExp(exp.left)
              val right = transpileExp(exp.right)
              val t = expType(exp)
              val tpe = transpileType(t)
              val temp = freshTempName()
              stmts = stmts :+ st"DeclNew$tpe($temp);"
              stmts = stmts :+ st"${tpe}_apply(sf, &$temp, $left, $right);"
              return st"(&$temp)"
            case _ =>
              val op: String = res.kind match {
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAdd => "__add"
                case AST.ResolvedInfo.BuiltIn.Kind.BinarySub => "__sub"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryMul => "__mul"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryDiv => "__div"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryRem => "__rem"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryEq => "__eq"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryNe => "__ne"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryLt => "__lt"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryLe => "__le"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryGt => "__gt"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryGe => "__ge"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryShl => "__shl"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryShr => "__shr"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryUshr => "__ushr"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAnd => "__and"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryOr => "__or"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryXor => "__xor"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAppend => "__append"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryPrepend => "__prepend"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryAppendAll => "__appendall"
                case AST.ResolvedInfo.BuiltIn.Kind.BinaryRemoveAll => "__removeall"
                case _ => halt("TODO") // TODO
              }
              val left = transpileExp(exp.left)
              val right = transpileExp(exp.right)
              val t = expType(exp.left) // TODO: trait eq
              return st"${transpileType(t)}$op($left, $right)"
          }
        case _ => halt("TODO") // TODO
      }
    }

    def transUnary(exp: AST.Exp.Unary): ST = {
      exp.attr.resOpt.get match {
        case res: AST.ResolvedInfo.BuiltIn =>
          if (res.kind == AST.ResolvedInfo.BuiltIn.Kind.UnaryNot) {
            val e = transpileExp(exp.exp)
            return st"(!$e)"
          } else {
            val tname = typeName(expType(exp))
            val op: String = res.kind match {
              case AST.ResolvedInfo.BuiltIn.Kind.UnaryComplement => "__complement"
              case AST.ResolvedInfo.BuiltIn.Kind.UnaryPlus => "__plus"
              case AST.ResolvedInfo.BuiltIn.Kind.UnaryMinus => "__minus"
              case _ => halt("Infeasible")
            }
            val e = transpileExp(exp.exp)
            return st"${mangleName(tname)}$op($e)"
          }
        case _ => halt(s"TODO: $exp") // TODO
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

    def transTuple(tuple: AST.Exp.Tuple): ST = {
      val tpe = transpileType(tuple.typedOpt.get)
      val temp = freshTempName()
      var args = ISZ[ST]()
      for (arg <- tuple.args) {
        val a = transpileExp(arg)
        args = args :+ a
      }
      stmts = stmts :+ st"DeclNew$tpe($temp);"
      stmts = stmts :+ st"${tpe}_apply(sf, &$temp, ${(args, ", ")});"
      return st"(&$temp)"
    }

    def transSelect(select: AST.Exp.Select): ST = {
      select.attr.resOpt.get match {
        case res: AST.ResolvedInfo.Tuple =>
          val receiver = select.receiverOpt.get
          val o = transpileExp(receiver)
          val index = res.index
          val t = expType(receiver).asInstanceOf[AST.Typed.Tuple]
          val tpe = transpileType(t)
          return st"${tpe}_$index($o)"
        case res: AST.ResolvedInfo.EnumElement => return elementName(res.owner, res.name)
        case res: AST.ResolvedInfo.BuiltIn =>
          res.kind match {
            case AST.ResolvedInfo.BuiltIn.Kind.EnumElements =>
              val owner = expType(select.receiverOpt.get).asInstanceOf[AST.Typed.Enum].name
              val iszType = transpileType(expType(select))
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
        case res: AST.ResolvedInfo.Method =>
          res.mode match {
            case AST.MethodMode.Method =>
              val t = expType(select)
              if (res.isInObject) {
                val r = transObjectMethodInvoke(t, res, ISZ())
                return r
              } else {
                val receiver = select.receiverOpt.get
                val receiverType = expType(receiver).asInstanceOf[AST.Typed.Name]
                val rcv = transpileExp(receiver)
                val r = transInstanceMethodInvoke(receiverType, t, res, rcv, ISZ())
                return r
              }
            case _ => halt(s"TODO: $res") // TODO
          }
        case res => halt(s"TODO: $res") // TODO
      }
    }

    def transObjectMethodInvoke(retType: AST.Typed, res: AST.ResolvedInfo.Method, invokeArgs: ISZ[AST.Exp]): ST = {
      var args = ISZ[ST]()
      for (arg <- invokeArgs) {
        val a = transpileExp(arg)
        args = args :+ a
      }
      if (isScalar(typeKind(retType)) || retType == AST.Typed.unit) {
        return st"${methodNameRes(None(), res)}(sf${commaArgs(args)})"
      } else {
        val temp = freshTempName()
        val tpe = transpileType(retType)
        stmts = stmts :+ st"DeclNew$tpe($temp);"
        stmts = stmts :+ st"${methodNameRes(None(), res)}(&$temp, sf${commaArgs(args)});"
        return st"(&$temp)"
      }
    }
    def transInstanceMethodInvoke(
      receiverType: AST.Typed.Name,
      retType: AST.Typed,
      res: AST.ResolvedInfo.Method,
      receiver: ST,
      invokeArgs: ISZ[AST.Exp]
    ): ST = {
      var args = ISZ[ST]()
      for (arg <- invokeArgs) {
        val a = transpileExp(arg)
        args = args :+ a
      }
      if (isScalar(typeKind(retType)) || retType == AST.Typed.unit) {
        return st"${methodNameRes(Some(receiverType), res)}(sf, $receiver${commaArgs(args)})"
      } else {
        val temp = freshTempName()
        val tpe = transpileType(retType)
        stmts = stmts :+ st"DeclNew$tpe($temp);"
        stmts = stmts :+ st"${methodNameRes(Some(receiverType), res)}(&$temp, sf, $receiver${commaArgs(args)});"
        return st"(&$temp)"
      }
    }

    def transInvoke(invoke: AST.Exp.Invoke): ST = {

      def transSApply(): ST = {
        val t = expType(invoke).asInstanceOf[AST.Typed.Name]
        val tpe = transpileType(t)
        val temp = freshTempName()
        val size = invoke.args.size
        val sizeType = arraySizeType(minIndexMaxElementSize(t.args(0), t.args(1))._2)
        stmts = stmts :+ st"""STATIC_ASSERT($size <= Max$tpe, "Insufficient maximum for $t elements.");"""
        stmts = stmts :+ st"DeclNew$tpe($temp);"
        stmts = stmts :+ st"$temp.size = ($sizeType) $size;"
        val targ = t.args(1)
        val targKind = typeKind(targ)
        if (isScalar(targKind)) {
          var i = 0
          for (arg <- invoke.args) {
            val a = transpileExp(arg)
            stmts = stmts :+ st"$temp.value[$i] = $a;"
            i = i + 1
          }
        } else {
          var i = 0
          for (arg <- invoke.args) {
            val a = transpileExp(arg)
            stmts = stmts :+ st"Type_assign(&$temp.value[$i], $a, sizeof(${typePrefix(targKind)}${transpileType(targ)}));"
            i = i + 1
          }
        }
        return st"(&$temp)"
      }

      def transReceiver(): ST = {
        invoke.receiverOpt match {
          case Some(receiver) => val r = transpileExp(receiver); return r
          case _ => val r = transpileExp(invoke.ident); return r
        }
      }

      def transSSelect(name: QName): ST = {
        val t = invoke.ident.typedOpt.get.asInstanceOf[AST.Typed.Name]
        val receiver = transReceiver()
        val arg = invoke.args(0)
        val tpe = transpileType(t)
        val e = transpileExp(arg)
        return st"${tpe}_at($receiver, $e)"
      }

      def transSStore(name: QName): ST = {
        val t = expType(invoke).asInstanceOf[AST.Typed.Name]
        val tpe = transpileType(t)
        val temp = freshTempName()
        val receiver = transReceiver()
        stmts = stmts :+ st"DeclNew$tpe($temp);"
        stmts = stmts :+ st"Type_assign(&$temp, $receiver, sizeof(struct $tpe));"
        val indexType = t.args(0)
        val elementType = t.args(1)
        val argType = AST.Typed.Tuple(ISZ(indexType, elementType))
        val argTpe = transpileType(argType)
        val argTypeKind = typeKind(elementType)
        if (isScalar(argTypeKind)) {
          for (arg <- invoke.args) {
            val e = transpileExp(arg)
            stmts = stmts :+ st"$temp.value[${argTpe}_1($e)] = ${argTpe}_2($e);"
          }
        } else {
          val elementTpe = transpileType(elementType)
          for (arg <- invoke.args) {
            val e = transpileExp(arg)
            stmts = stmts :+ st"Type_assign(&$temp.value[${argTpe}_1($e)], ${argTpe}_2($e), sizeof(${typePrefix(argTypeKind)}$elementTpe));"
          }
        }
        return st"(&$temp)"
      }

      def transExt(res: AST.ResolvedInfo.Method): ST = {
        val t = expType(invoke)
        val tpe = transpileType(t)
        var args = ISZ[ST]()
        for (arg <- invoke.args) {
          val a = transpileExp(arg)
          args = args :+ a
        }
        val name: ST = if (sNameSet.contains(res.owner)) st"${tpe}_${res.id}" else methodNameRes(None(), res)
        if (isScalar(typeKind(t)) || t == AST.Typed.unit) {
          return st"$name(sf${commaArgs(args)})"
        } else {
          val temp = freshTempName()
          stmts = stmts :+ st"DeclNew$tpe($temp);"
          stmts = stmts :+ st"$name(&$temp, sf${commaArgs(args)});"
          return st"(&$temp)"
        }
      }
      def transConstructor(method: AST.ResolvedInfo.Method): ST = {
        val t = expType(invoke)
        val tpe = transpileType(t)
        val temp = freshTempName()
        var args = ISZ[ST]()
        for (arg <- invoke.args) {
          val a = transpileExp(arg)
          args = args :+ a
        }
        stmts = stmts :+ st"DeclNew$tpe($temp);"
        stmts = stmts :+ st"${tpe}_apply(sf, &$temp${commaArgs(args)});"
        return st"(&$temp)"
      }

      invoke.attr.resOpt.get match {
        case res: AST.ResolvedInfo.Method =>
          res.mode match {
            case AST.MethodMode.Method =>
              if (res.isInObject) {
                val r = transObjectMethodInvoke(expType(invoke), res, invoke.args)
                return r
              } else {
                val receiver = transReceiver()
                val r = transInstanceMethodInvoke(
                  expType(invoke.receiverOpt.get).asInstanceOf[AST.Typed.Name],
                  expType(invoke),
                  res,
                  receiver,
                  invoke.args
                )
                return r
              }
            case AST.MethodMode.Spec => halt(s"TODO: $res") // TODO
            case AST.MethodMode.Ext => val r = transExt(res); return r
            case AST.MethodMode.Constructor =>
              res.owner :+ res.id match {
                case AST.Typed.isName => val r = transSApply(); return r
                case AST.Typed.msName => val r = transSApply(); return r
                case _ => val r = transConstructor(res); return r
              }
            case AST.MethodMode.Copy => halt(s"TODO: $res") // TODO
            case AST.MethodMode.Extractor => halt(s"TODO: $res") // TODO
            case AST.MethodMode.ObjectConstructor => halt(s"TODO: $res") // TODO
            case AST.MethodMode.Select => val r = transSSelect(res.owner :+ res.id); return r
            case AST.MethodMode.Store => val r = transSStore(res.owner :+ res.id); return r
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
      case exp: AST.Exp.StringInterpolate =>
        if (isSubZLit(exp)) {
          val r = transSubZLit(exp)
          return r
        } else {
          halt(s"// TODO: $exp") // TODO
        }
      case exp: AST.Exp.LitString => val r = transLitString(exp); return r
      case exp: AST.Exp.Ident => val r = transIdent(exp); return r
      case exp: AST.Exp.Binary => val r = transBinary(exp); return r
      case exp: AST.Exp.Unary => val r = transUnary(exp); return r
      case exp: AST.Exp.Select => val r = transSelect(exp); return r
      case exp: AST.Exp.Tuple => val r = transTuple(exp); return r
      case exp: AST.Exp.Invoke => val r = transInvoke(exp); return r
      case _ => halt(s"TODO: $exp") // TODO
    }
  }

  def transToString(s: ST, exp: AST.Exp): Unit = {
    val tmp = transpileExp(exp)
    val mangledName = transpileType(expType(exp))
    stmts = stmts :+ st"${mangledName}_string($s, sf, $tmp);"
  }

  def transPrintH(isOut: ST, exp: AST.Exp): Unit = {
    val tmp = transpileExp(exp)
    val mangledName = transpileType(expType(exp))
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

    def transVar(stmt: AST.Stmt.Var): Unit = {
      transpileLoc(stmt)
      val init = stmt.initOpt.get
      val t: AST.Typed = stmt.tipeOpt match {
        case Some(tipe) => tipe.typedOpt.get
        case _ => init.asInstanceOf[AST.Stmt.Expr].typedOpt.get
      }
      val local = localName(stmt.id.value)
      val tpe = transpileType(t)
      init match {
        case _: AST.Stmt.Expr =>
          if (isScalar(typeKind(t)) || stmt.isVal) {
            transpileAssignExp(init, (rhs, _) => st"$tpe $local = $rhs;")
          } else {
            stmts = stmts :+ st"DeclNew$tpe(_$local);"
            transpileAssignExp(init, (rhs, _) => st"$tpe $local = $rhs;")
          }
        case _ =>
          if (isScalar(typeKind(t)) && !stmt.isVal) {
            stmts = stmts :+ st"$tpe $local;"
            transpileAssignExp(init, (rhs, _) => st"$local = $rhs;")
          } else {
            stmts = stmts :+ st"DeclNew$tpe(_$local);"
            stmts = stmts :+ st"$tpe $local = ($tpe) &_$local;"
            transpileAssignExp(init, (rhs, rhsT) => st"Type_assign(&$local, $rhs, sizeof($rhsT));")
          }
      }
    }

    def transAssign(stmt: AST.Stmt.Assign): Unit = {
      transpileLoc(stmt)
      val t = expType(stmt.lhs)
      if (isScalar(typeKind(t))) {
        stmt.lhs match {
          case lhs: AST.Exp.Ident =>
            lhs.attr.resOpt.get match {
              case res: AST.ResolvedInfo.LocalVar =>
                res.scope match {
                  case AST.ResolvedInfo.LocalVar.Scope.Closure => halt("TODO") // TODO
                  case _ =>
                    transpileAssignExp(stmt.rhs, (rhs, _) => st"${localName(lhs.id.value)} = $rhs;")
                }
              case res: AST.ResolvedInfo.Var =>
                if (res.isInObject) {
                  val name = mangleName(res.owner :+ res.id)
                  transpileAssignExp(stmt.rhs, (rhs, _) => st"${name}_a(sf, $rhs);")
                } else {
                  val t = currReceiverOpt.get
                  transpileAssignExp(stmt.rhs, (rhs, _) => st"${transpileType(t)}_${fieldName(res.id)}_a(this, $rhs);")
                }
              case _ => halt("Infeasible")
            }
          case lhs: AST.Exp.Select => halt(s"TODO: $lhs") // TODO
          case lhs: AST.Exp.Invoke =>
            val (receiverType, receiver): (AST.Typed.Name, ST) = lhs.receiverOpt match {
              case Some(rcv) => val r = transpileExp(rcv); (expType(rcv).asInstanceOf[AST.Typed.Name], r)
              case _ => val r = transpileExp(lhs.ident); (expType(lhs.ident).asInstanceOf[AST.Typed.Name], r)
            }
            val et = receiverType.args(1)
            val index = transpileExp(lhs.args(0))
            if (isScalar(typeKind(et))) {
              transpileAssignExp(stmt.rhs, (rhs, _) => st"${transpileType(receiverType)}_at($receiver, $index) = $rhs;")
            } else {
              transpileAssignExp(
                stmt.rhs,
                (rhs, _) =>
                  st"Type_assign(${transpileType(receiverType)}_at($receiver, $index), $rhs, sizeof(${typeDecl(et)}));"
              )
            }
          case _ => halt("Infeasible")
        }
      } else {
        halt(s"TODO: $stmt") // TODO
      }
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
      stmts = stmts :+ st"B $t2 = $t;"
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

    def transpileFor(stmt: AST.Stmt.For): Unit = {
      def transEnumGen(eg: AST.EnumGen.For, body: ISZ[ST]): ISZ[ST] = {
        eg.range match {
          case range: AST.EnumGen.Range.Step =>
            stmts = ISZ()
            val start = transpileExp(range.start)
            val end = transpileExp(range.end)
            val (by, byE): (ST, Either[Z, ST]) = range.byOpt match {
              case Some(byExp) =>
                byExp match {
                  case byExp: AST.Exp.LitZ => val v = byExp.value; (st"$v", Either.Left(v))
                  case _ => val v = transpileExp(byExp); (v, Either.Right(v))
                }
              case _ => (st"1", Either.Left(1))
            }
            val id: ST = eg.idOpt match {
              case Some(x) => st"${x.value}"
              case _ => freshTempName()
            }
            val tpe = transpileType(expType(range.start))
            stmts = stmts :+ st"$tpe $id = $start;"
            val endTemp = freshTempName()
            stmts = stmts :+ st"$tpe $endTemp = $end;"
            val byTemp = freshTempName()
            stmts = stmts :+ st"Z $byTemp = $by;"
            val b: ISZ[ST] = eg.condOpt match {
              case Some(cond) =>
                val oldStmts = stmts
                stmts = ISZ()
                val c = transpileExp(cond)
                stmts = oldStmts
                ISZ(st"""if ($c) {
                |  ${(body, "\n")}
                |}""")
              case _ => body
            }
            val pos =
              st"""while ($id ${if (range.isInclusive) "<=" else "<"} $endTemp) {
              |  ${(b, "\n")}
              |  $id = ($tpe) ($id + $byTemp);
              |}"""
            val neg =
              st"""while ($id ${if (range.isInclusive) ">=" else ">"} $endTemp) {
              |  ${(b, "\n")}
              |  $id = ($tpe) ($id + $byTemp);
              |}"""
            byE match {
              case Either.Left(n) =>
                if (n > 0) {
                  stmts = stmts :+ pos
                } else {
                  stmts = stmts :+ neg
                }
              case _ =>
                stmts = stmts :+
                  st"""if ($byTemp > 0) {
                  |  $pos
                  |} else {
                  |  $neg
                  |}"""
            }
            return stmts
          case range: AST.EnumGen.Range.Expr => halt(s"TODO: $range") // TODO
        }
      }
      transpileLoc(stmt)
      val oldStmts = stmts
      stmts = ISZ()
      for (stmt <- stmt.body.stmts) {
        transpileStmt(stmt)
      }
      val egs = stmt.enumGens
      var body = transEnumGen(egs(egs.size - 1), stmts)
      for (i <- (egs.size - 2) to z"0" by -1) {
        body = transEnumGen(egs(i), body)
      }
      stmts = oldStmts :+
        st"""{
        |  ${(body, "\n")}
        |}"""
    }

    def transpileReturn(stmt: AST.Stmt.Return): Unit = {
      stmt.expOpt match {
        case Some(exp) =>
          val t = expType(exp)
          val e = transpileExp(exp)
          if (isScalar(typeKind(t))) {
            stmts = stmts :+ st"return $e;"
          } else {
            stmts = stmts :+ st"Type_assign(result, $e, sizeof(${typeDecl(t)}));"
          }
        case _ => stmts = stmts :+ st"return;"
      }
    }

    stmt match {
      case stmt: AST.Stmt.Var => transVar(stmt)
      case stmt: AST.Stmt.Assign => transAssign(stmt)
      case stmt: AST.Stmt.Expr =>
        stmt.exp match {
          case exp: AST.Exp.Invoke =>
            if (isBuiltInStmt(exp)) {
              exp.attr.resOpt.get.asInstanceOf[AST.ResolvedInfo.BuiltIn].kind match {
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
            } else {
              stmts = stmts :+ st"${transpileExp(exp)};"
            }
          case exp => halt(s"Infeasible: $exp")
        }
      case stmt: AST.Stmt.VarPattern => transpileVarPattern(stmt) // TODO
      case stmt: AST.Stmt.Block => transpileBlock(stmt)
      case stmt: AST.Stmt.If => transpileIf(stmt)
      case stmt: AST.Stmt.While => transpileWhile(stmt)
      case stmt: AST.Stmt.DoWhile => transpileDoWhile(stmt)
      case stmt: AST.Stmt.Match => transpileMatch(stmt)
      case stmt: AST.Stmt.For => transpileFor(stmt)
      case stmt: AST.Stmt.Return => transpileReturn(stmt)
      case _: AST.Stmt.Import => // skip
      case _: AST.Stmt.AbstractDatatype => // skip
      case _: AST.Stmt.Sig => // skip
      case _: AST.Stmt.Enum => // skip
      case _: AST.Stmt.Object => // skip
      case _: AST.Stmt.SpecMethod => // skip
      case _: AST.Stmt.SpecVar => // skip
      case _: AST.Stmt.TypeAlias => // skip
      case _ => halt(s"TODO: $stmt") // TODO
    }
  }

  @pure def transpileType(tpe: AST.Typed): ST = {
    return st"${typeNameMap.get(tpe).get}"
  }

  @pure def typeDecl(t: AST.Typed): ST = {
    val kind = typeKind(t)
    return if (t == AST.Typed.string) st"struct StaticString" else st"${typePrefix(kind)}${fingerprint(t)._1}"
  }

  @memoize def typeKind(t: AST.Typed): TypeKind.Type = {
    @pure def bitWidthKind(n: Z): TypeKind.Type = {
      n match {
        case z"8" => return TypeKind.Scalar8
        case z"16" => return TypeKind.Scalar16
        case z"32" => return TypeKind.Scalar32
        case z"64" => return TypeKind.Scalar64
        case _ => halt(s"Infeasible: $n")
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
            case info: TypeInfo.SubZ =>
              val bw = info.ast.bitWidth
              return bitWidthKind(if (bw == z"0") config.defaultBitWidth else bw)
            case _: TypeInfo.Enum => return TypeKind.Enum
            case info: TypeInfo.AbstractDatatype =>
              return if (info.ast.isDatatype) if (info.ast.isRoot) TypeKind.ImmutableTrait else TypeKind.Immutable
              else if (info.ast.isRoot) TypeKind.MutableTrait
              else TypeKind.Mutable
            case info: TypeInfo.Sig =>
              return if (info.ast.isExt) TypeKind.Immutable
              else if (info.ast.isImmutable) TypeKind.ImmutableTrait
              else TypeKind.MutableTrait
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

  @pure def methodNameRes(receiverTypeOpt: Option[AST.Typed.Name], res: AST.ResolvedInfo.Method): ST = {
    return methodName(receiverTypeOpt, res.isInObject, res.owner, res.id, res.typeParams.isEmpty, res.tpeOpt.get)
  }

  @pure def methodName(
    receiverTypeOpt: Option[AST.Typed.Name],
    isInObject: B,
    owner: QName,
    id: String,
    noTypeParams: B,
    t: AST.Typed.Fun
  ): ST = {
    val ids = owner
    val r: ST =
      if (isInObject)
        if (noTypeParams) mangleName(ids :+ id)
        else st"${mangleName(ids :+ id)}_${fprint(t)}"
      else if (sNameSet.contains(ids)) st"${transpileType(receiverTypeOpt.get)}_$id"
      else if (noTypeParams) st"${transpileType(receiverTypeOpt.get)}_${id}_"
      else st"${transpileType(receiverTypeOpt.get)}_${id}_${fprint(t)}_"
    return r
  }

  @pure def methodHeaderRes(receiverOpt: Option[AST.Typed.Name], res: AST.ResolvedInfo.Method): ST = {
    return methodHeader(
      receiverOpt,
      res.isInObject,
      res.owner,
      res.id,
      res.typeParams.isEmpty,
      res.tpeOpt.get,
      res.paramNames
    )
  }

  @pure def methodHeader(
    receiverOpt: Option[AST.Typed.Name],
    isInObject: B,
    owner: QName,
    id: String,
    noTypeParams: B,
    t: AST.Typed.Fun,
    paramNames: ISZ[String]
  ): ST = {
    val name = methodName(receiverOpt, isInObject, owner, id, noTypeParams, t)
    val tpe = transpileType(t.ret)
    val (retType, retTypeDecl): (ST, ST) =
      if (isScalar(typeKind(t.ret)) || t.ret == AST.Typed.unit) (st"", tpe)
      else (st"$tpe result, ", st"void")
    val preParams: ST = receiverOpt match {
      case Some(receiver) => st"${retType}StackFrame caller, ${transpileType(receiver)} this"
      case _ => st"${retType}StackFrame caller"
    }
    val params: ST =
      if (paramNames.isEmpty) preParams
      else
        st"$preParams, ${(
          for (p <- ops.ISZOps(t.args).zip(paramNames))
            yield st"${transpileType(p._1)} ${localName(p._2)}",
          ", "
        )}"
    return st"$retTypeDecl $name($params)"
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
