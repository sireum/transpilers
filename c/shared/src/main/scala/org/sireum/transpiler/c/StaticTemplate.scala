// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol.Resolver._
import org.sireum.message._

object StaticTemplate {

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

  @datatype class Compiled(typeHeader: ISZ[ST], header: ISZ[ST], impl: ISZ[ST])

  @datatype class Vard(kind: TypeKind.Type, id: String, tpe: ST, tpePtr: ST, isVar: B, isHidden: B)

  val sireumDir: String = "sireum"
  val libraryDir: String = "library"
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

  val keywords: HashSet[String] = HashSet ++ ISZ(
    "auto",
    "double",
    "int",
    "struct",
    "break",
    "else",
    "long",
    "switch",
    "case",
    "enum",
    "register",
    "typedef",
    "char",
    "extern",
    "return",
    "union",
    "const",
    "float",
    "short",
    "unsigned",
    "continue",
    "for",
    "signed",
    "void",
    "default",
    "goto",
    "sizeof",
    "volatile",
    "do",
    "if",
    "static",
    "while"
  )

  @pure def typeCompositeH(stringMax: Z, isStringMax: Z, typeNames: ISZ[(String, ST, ST)]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_TYPE_H
      |#define SIREUM_GEN_TYPE_H
      |
      |#include <memory.h>
      |#include <ztype.h>
      |#include <stackframe.h>
      |
      |typedef enum {
      |  T${typeNames(0)._2} = 0, // ${typeNames(0)._1}
      |  ${(
        for (tn <- ops.ISZOps(ops.ISZOps(typeNames).zip(typeNames.indices.map(n => n))).drop(1))
          yield st"T${tn._1._2} = ${tn._2}, // ${tn._1._1}",
        "\n"
      )}
      |} TYPE;
      |
      |char *TYPE_string(void *type);
      |
      |typedef struct Type *Type;
      |struct Type {
      |  TYPE type;
      |};
      |
      |#define MaxString $stringMax
      |
      |typedef struct String *String;
      |struct String {
      |  TYPE type;
      |  Z size;
      |  C value[];
      |};
      |
      |struct StaticString {
      |  TYPE type;
      |  Z size;
      |  C value[MaxString + 1];
      |};
      |
      |#define string(v) (String) &((struct { TYPE type; Z size; C value[sizeof(v)]; }) { TString, Z_C(sizeof (v) - 1), v })
      |#define DeclNewString(x) struct StaticString x = { .type = TString }
      |
      |static inline B String__eq(String this, String other) {
      |  Z thisSize = this->size;
      |  if (thisSize != other->size) return F;
      |  return memcmp(this->value, other->value, (size_t) thisSize) == 0;
      |}
      |
      |static inline B String__ne(String this, String other) {
      |  return !String__eq(this, other);
      |}
      |
      |#endif"""
    return r
  }

  @pure def typesH(names: ISZ[QName], typeNames: ISZ[(String, ST, ST)]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_H
      |#define SIREUM_GEN_H
      |
      |#include <misc.h>
      |${(for (name <- ops.ISZOps(names).sortWith(qnameLt)) yield st"#include <type-${(name, "_")}.h>", "\n")}
      |
      |#if defined(static_assert)
      |#define STATIC_ASSERT static_assert
      |#define GLOBAL_STATIC_ASSERT(a, b, c) static_assert(b, c)
      |#else
      |#define STATIC_ASSERT(pred, explanation); {char assert[1/(pred)];(void)assert;}
      |#define GLOBAL_STATIC_ASSERT(unique, pred, explanation); namespace ASSERTION {char unique[1/(pred)];}
      |#endif
      |
      |static inline size_t sizeOf(Type t) {
      |  TYPE type = t->type;
      |  switch (type) {
      |    ${(for (tn <- typeNames) yield st"case T${tn._2}: return sizeof(struct ${tn._2}); // ${tn._1}", "\n")}
      |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
      |  }
      |}
      |
      |void Type_assign(void *dest, void *src, size_t destSize);
      |
      |#endif"""
    return r
  }

  @pure def typesC(typeNames: ISZ[(String, ST, ST)]): ST = {
    val strings: ISZ[ST] = for (tn <- typeNames) yield st""""${tn._3}""""
    val r =
      st"""#include <types.h>
      |
      |void Type_assign(void *dest, void *src, size_t destSize) {
      |  Type srcType = (Type) src;
      |  if (srcType->type == TString) {
      |    String_assign((String) dest, (String) src);
      |    return;
      |  }
      |  size_t srcSize = sizeOf(srcType);
      |  memcpy(dest, src, srcSize);
      |  memset(((char *) dest) + srcSize, 0, destSize - srcSize);
      |}
      |
      |char *TYPE_string(void *type) {
      |  static char *strings[] = {
      |    ${(strings, ",\n")}
      |  };
      |  return strings[((Type) type)->type];
      |}"""
    return r
  }

  @pure def allH(names: ISZ[QName]): ST = {
    val r =
      st"""#ifndef SIREUM_ALL_H
      |#define SIREUM_ALL_H
      |
      |#include <types.h>
      |${(for (name <- ops.ISZOps(names).sortWith(qnameLt)) yield st"#include <${(name, "_")}.h>", "\n")}
      |
      |B Type__eq(void *t1, void *t2);
      |void Type_cprint(void *this, B isOut);
      |void Type_string(String result, StackFrame caller, void* this);
      |
      |#endif"""
    return r
  }

  @pure def allC(typeNames: ISZ[(String, ST, ST)]): ST = {
    val r =
      st"""#include <all.h>
      |
      |B Type__eq(void *t1, void *t2) {
      |  TYPE type = ((Type) t1)->type;
      |  if (type != ((Type) t2)->type) return F;
      |  switch (type) {
      |    ${(for (tn <- typeNames) yield st"case T${tn._2}: return ${tn._2}__eq((${tn._2}) t1, (${tn._2}) t2);", "\n")}
      |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
      |  }
      |}
      |
      |void Type_cprint(void *this, B isOut) {
      |  TYPE type = ((Type) this)->type;
      |  switch (type) {
      |    ${(for (tn <- typeNames) yield st"case T${tn._2}: ${tn._2}_cprint((${tn._2}) this, isOut); return;", "\n")}
      |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
      |  }
      |}
      |
      |void Type_string(String result, StackFrame caller, void *this) {
      |  TYPE type = ((Type) this)->type;
      |  switch (type) {
      |    ${(
        for (tn <- typeNames) yield st"case T${tn._2}: ${tn._2}_string(result, caller, (${tn._2}) this); return;",
        "\n"
      )}
      |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
      |  }
      |}"""
    return r
  }

  @pure def cmake(project: String, mainFilenames: ISZ[String], filess: ISZ[QName]): ST = {
    @pure def files(filess: ISZ[QName]): ISZ[ST] = {
      return for (f <- filess)
        yield if (f.size == z"1") st"${f(0)}" else st"${(ops.ISZOps(f).dropRight(1), "/")}/${f(f.size - 1)}"
    }

    @pure def includeDirs(filess: ISZ[QName]): ISZ[ST] = {
      var r = HashSSet.empty[QName]
      for (f <- filess) {
        if (f.size == z"1") {
          r = r + ISZ(".")
        } else {
          r = r + ops.ISZOps(f).dropRight(1)
        }
      }
      return for (f <- r.elements) yield st"PUBLIC ${(f, "/")}"
    }

    @pure def target(filename: String): ST = {
      val r =
        st"""
        |add_executable($filename
        |        ${(files(filess), "\n")})
        |
        |target_include_directories($filename
        |        ${(includeDirs(filess), "\n")})"""
      return r
    }

    val mains: ISZ[ST] = for (f <- mainFilenames) yield target(f)

    val r =
      st"""cmake_minimum_required(VERSION 3.9)
      |
      |project($project)
      |
      |set(CMAKE_C_STANDARD 99)
      |
      |add_compile_options(-Werror)
      |
      |add_compile_options("$$<$$<CONFIG:Release>:-O2>")
      |
      |${(mains, "\n\n")}"""
    return r
  }

  @pure def typeManglingMap(entries: ISZ[(String, String)]): ST = {
    val sortedEntries = ops.ISZOps(entries).sortWith((p1, p2) => p1._1 <= p2._2)
    return st"${(for (p <- sortedEntries) yield st"${p._1}=${p._2}", "\n")}"
  }

  @pure def qnameLt(qn1: QName, qn2: QName): B = {
    return if (qn1.size < qn2.size) T else if (qn1.size > qn1.size) F else st"$qn1".render <= st"$qn2".render
  }

  @pure def dirOf(name: QName): QName = {
    return if (name.size == z"1") ISZ(libraryDir) else libraryDir +: ops.ISZOps(name).dropRight(1)
  }

  @pure def filenameOf(name: QName): ST = {
    return st"${(name, "_")}"
  }

  @pure def typeHeaderFilename(filename: ST): ST = {
    return st"type-$filename.h"
  }

  @pure def compiled(compiledMap: HashMap[QName, Compiled]): ISZ[(QName, ST)] = {
    val entries = compiledMap.entries
    val sortedEntries = ops.ISZOps(entries).sortWith((p1, p2) => qnameLt(p1._1, p2._1))
    var r = ISZ[(QName, ST)]()
    for (e <- sortedEntries) {
      val qname = e._1
      val dir = dirOf(qname)
      val filename = filenameOf(qname)
      val comp = e._2
      val tHeaderFilename = typeHeaderFilename(filename).render
      val headerFilename = st"$filename.h".render
      val implFilename = st"$filename.c".render
      r = r :+ ((dir :+ tHeaderFilename, st"""#ifndef SIREUM_TYPE_H_$filename
      |#define SIREUM_TYPE_H_$filename
      |#include <type.h>
      |
      |${(comp.typeHeader, "\n\n")}
      |
      |#endif"""))
      r = r :+ ((dir :+ headerFilename, st"""#ifndef SIREUM_H_$filename
      |#define SIREUM_H_$filename
      |#include <types.h>
      |
      |${(comp.header, "\n\n")}
      |
      |#endif"""))
      r = r :+ ((dir :+ implFilename, st"""#include <all.h>
      |
      |${(comp.impl, "\n\n")}"""))
    }
    return r
  }

  @pure def worksheet(filename: String, stmts: ISZ[ST]): ST = {
    val r =
      st"""#include <all.h>
      |
      |int main(void) {
      |  DeclNewStackFrame(NULL, "$filename", "<worksheet>", "<main>", 0);
      |
      |  ${(stmts, "\n")}
      |
      |  return 0;
      |}"""
    return r
  }

  @pure def main(
    filename: String,
    owner: QName,
    id: String,
    iszStringType: ST,
    iszSizeType: String,
    atExit: ISZ[ST]
  ): ST = {
    val r =
      st"""#include <all.h>
      |
      |void atExit(void) {
      |  ${(atExit, "\n")}
      |}
      |
      |int main(int argc, char *argv[]) {
      |  atexit(atExit);
      |
      |  DeclNewStackFrame(NULL, "$filename", "${dotName(owner)}", "<App>", 0);
      |
      |  DeclNew$iszStringType(t_args);
      |
      |  int size = argc - 1;
      |  if (size > Max$iszStringType) {
      |    sfAbort("Argument list too long.");
      |  }
      |
      |  for (int i = 0; i < size; i++) {
      |    char *arg = argv[i + 1];
      |    size_t argSize = strlen(arg);
      |    if (argSize > MaxString) {
      |      sfAbort("Argument too long.");
      |    }
      |    ${iszStringType}_at(&t_args, i)->size = (Z) argSize;
      |    memcpy(${iszStringType}_at(&t_args, i)->value, arg, argSize + 1);
      |  }
      |
      |  t_args.size = ($iszSizeType) size;
      |
      |  return (int) ${mangleName(owner)}_$id(sf, &t_args);
      |}"""
    return r
  }

  @pure def claszConstructor(
    compiled: Compiled,
    uri: String,
    className: QName,
    name: ST,
    constructorParamTypes: ISZ[(TypeKind.Type, String, ST, ST)],
    constructorStmts: ISZ[ST]
  ): Compiled = {
    val constructorParams: ST = commaArgs(for (q <- constructorParamTypes) yield st"${q._4} ${q._2}")
    val constructorHeader = st"void ${name}_apply(StackFrame caller, $name this$constructorParams)"
    val constructorInits: ISZ[ST] = for (q <- constructorParamTypes)
      yield
        if (isScalar(q._1)) st"this->${q._2} = ${q._2};"
        else st"Type_assign(&this->${q._2}, ${q._2}, sizeof(${q._3}));"
    return compiled(
      header = compiled.header :+ st"$constructorHeader;",
      impl = compiled.impl :+
        st"""$constructorHeader {
        |  DeclNewStackFrame(caller, "$uri", "${dotName(className)}", "apply", 0);
        |  ${(constructorInits, "\n")}
        |  ${(constructorStmts, "\n")}
        |}"""
    )
  }

  @pure def clasz(
    compiled: Compiled,
    uri: String,
    className: QName,
    includes: ISZ[ST],
    tpe: String,
    name: ST,
    constructorParamTypes: ISZ[Vard],
    varTypes: ISZ[Vard]
  ): Compiled = {
    val vars = constructorParamTypes ++ varTypes
    var members = ISZ[ST]()
    for (vd <- ops.ISZOps(vars).sortWith((vd1, vd2) => vd1.kind.ordinal < vd2.kind.ordinal)) {
      members = members :+ st"${vd.tpe} ${vd.id};"
    }
    val typeHeader =
      st"""// $tpe
      |${(includes, "\n")}
      |
      |typedef struct $name *$name;
      |struct $name {
      |  TYPE type;
      |  ${(members, "\n")}
      |};
      |
      |#define DeclNew$name(x) struct $name x = { .type = T$name }
      |"""

    var accessors = ISZ[ST]()
    for (vd <- vars) {
      if (isScalar(vd.kind)) {
        accessors = accessors :+ st"#define ${name}_${vd.id}_(this) ((this)->${vd.id})"
        if (vd.isVar) {
          accessors = accessors :+ st"#define ${name}_${vd.id}_a(this, value) (this)->${vd.id} = (value)"
        }
      } else {
        accessors = accessors :+ st"#define ${name}_${vd.id}_(this) ((${vd.tpePtr}) &(this)->${vd.id})"
        if (vd.isVar) {
          accessors = accessors :+ st"#define ${name}_${vd.id}_a(this, value) Type_assign((this)->${vd.id}, value, sizeof(${vd.tpe}))"
        }
      }
    }

    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val stringHeader = st"void ${name}_string(String result, StackFrame caller, $name this)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"

    val header =
      st"""// $tpe
      |
      |${(accessors, "\n")}
      |
      |$eqHeader;
      |$cprintHeader;
      |$stringHeader;
      |
      |#define ${name}__is(this) ((($name) this)->type == T$name)
      |
      |static inline $name ${name}__as(StackFrame caller, void *this) {
      |  if (${name}__is(this)) return ($name) this;
      |  fprintf(stderr, "Invalid case from %s to $tpe.", TYPE_string(this));
      |  sfAbortImpl(caller, "");
      |  $abort
      |}"""

    var eqStmts = ISZ[ST]()
    var stringStmts = ISZ[ST](st"""DeclNewStackFrame(caller, "$uri", "${dotName(className)}", "string", 0);
    |String_string(result, sf, string("${className(className.size - 1)}("));""")
    var cprintStmts = ISZ[ST](st"""String_cprint(string("${className(className.size - 1)}("), isOut);""")

    if (constructorParamTypes.size > 1) {
      stringStmts = stringStmts :+ st"""String sep = string(", ");"""
      cprintStmts = cprintStmts :+ st"""String sep = string(", ");"""
    }

    for (i <- constructorParamTypes.indices) {
      val vd = constructorParamTypes(i)
      val pre: ST = if (isScalar(vd.kind)) st"" else st"(${vd.tpePtr}) &"
      if (!vd.isHidden) {
        eqStmts = eqStmts :+ st"if (${vd.tpePtr}__ne(${pre}this->${vd.id}, ${pre}other->${vd.id})) return F;"
      }
      if (i > 0) {
        stringStmts = stringStmts :+ st"String_string(result, sf, sep);"
        cprintStmts = cprintStmts :+ st"String_cprint(sep, isOut);"
      }
      stringStmts = stringStmts :+ st"${vd.tpePtr}_string(result, sf, ${pre}this->${vd.id});"
      cprintStmts = cprintStmts :+ st"${vd.tpePtr}_cprint(${pre}this->${vd.id}, isOut);"
    }
    stringStmts = stringStmts :+ st"""String_string(result, sf, string(")"));"""
    cprintStmts = cprintStmts :+ st"""String_cprint(string(")"), isOut);"""

    val impl =
      st"""// $tpe
      |
      |$eqHeader {
      |  ${(eqStmts, "\n")}
      |  return T;
      |}
      |
      |$stringHeader {
      |  ${(stringStmts, "\n")}
      |}
      |
      |$cprintHeader {
      |  ${(cprintStmts, "\n")}
      |}"""

    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ header,
      impl = compiled.impl :+ impl
    )
  }

  @pure def traitz(compiled: Compiled, name: ST, tpe: String, includes: ISZ[ST], leafTypes: ISZ[ST]): Compiled = {
    val typeHeader =
      st"""// $tpe
      |
      |${(includes, "\n")}
      |
      |typedef union $name *$name;
      |union $name {
      |  TYPE type;
      |  ${(for (t <- leafTypes) yield st"struct $t $t;", "\n")}
      |};
      |
      |#define DeclNew$name(x) union $name x = { 0 }"""
    val header =
      st"""// $tpe
      |
      |#define ${name}__eq(this, other) Type__eq(this, other)
      |#define ${name}__ne(this, other) (!Type__eq(this, other))
      |#define ${name}_cprint(this, isOut) Type_cprint(this, isOut)
      |#define ${name}_string(result, caller, this) Type_string(result, caller, this)
      |
      |B ${name}__is(void *this);
      |$name ${name}__as(StackFrame caller, void *this);"""
    val impl =
      st"""// $tpe
      |
      |B ${name}__is(void *this) {
      |  switch(((Type) this)->type) {
      |    ${(for (t <- leafTypes) yield st"case T$t: return T;", "\n")}
      |    default: return F;
      |  }
      |}
      |
      |$name ${name}__as(StackFrame caller, void *this) {
      |  switch(((Type) this)->type) {
      |    ${(for (t <- leafTypes) yield st"case T$t: break;", "\n")}
      |    default:
      |      fprintf(stderr, "Invalid cast from %s to $tpe.", TYPE_string(this));
      |      sfAbortImpl(caller, "");
      |  }
      |  return ($name) this;
      |}"""
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ header,
      impl = compiled.impl :+ impl
    )
  }

  @pure def arraySizeType(maxElement: Z): String = {
    return if (maxElement < i8Max) "int8_t"
    else if (maxElement < i16Max) "int16_t"
    else if (maxElement < i32Max) "int32_t"
    else if (maxElement < i64Max) "int64_t"
    else "intmax_t"
  }

  @pure def array(
    compiled: Compiled,
    includes: ISZ[ST],
    tpe: String,
    isImmutable: B,
    name: ST,
    otherNameOpt: Option[ST],
    indexType: ST,
    minIndex: Z,
    isElementTypeScalar: B,
    elementType: ST,
    elementTypePtr: ST,
    maxElement: Z
  ): Compiled = {
    val offset: ST = if (minIndex == z"0") st"" else st"- ${indexType}_C($minIndex)"
    val sName: String = if (isImmutable) "IS" else "MS"
    val sizeType = arraySizeType(maxElement)
    val at: ST =
      if (isElementTypeScalar) st"#define ${name}_at(this, i) ((this)->value[($sizeType) (i)$offset])"
      else st"#define ${name}_at(this, i) (($elementTypePtr) &((this)->value[($sizeType) (i)$offset]))"
    val toOtherOpt: Option[ST] = otherNameOpt match {
      case Some(otherName) =>
        val other: String = if (isImmutable) "MS" else "IS"
        Some(st"""
        |static inline void ${name}_to$other($otherName result, StackFrame caller, $name this) {
        |  STATIC_ASSERT(Max$otherName >= Max$name, "Invalid cast from $tpe to $other[...,...].");
        |  result->type = T$otherName;
        |  result->size = this->size;
        |  memcpy(&result->value, &this->value, this->size * sizeof($elementType));
        |}""")
      case _ => None()
    }
    val typeHeader =
      st"""// $tpe
      |${(includes, "\n")}
      |
      |#define Max$name $maxElement
      |#define ${name}SizeT $sizeType

      |typedef struct $name *$name;
      |struct $name {
      |  TYPE type;
      |  $sizeType size;
      |  $elementType value[Max$name];
      |};
      |
      |#define DeclNew$name(x) struct $name x = { .type = T$name }
      |#define ${name}_size(sf, this) (($indexType) (this)->size)
      |#define ${name}_zize(sf, this) ((Z) (this)->size)
      |$at"""
    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val createHeader = st"void ${name}_create($name result, StackFrame caller, $indexType size, $elementTypePtr dflt)"
    val zreateHeader = st"void ${name}_zreate($name result, StackFrame caller, Z size, $elementTypePtr dflt)"
    val appendHeader = st"void ${name}__append($name result, StackFrame caller, $name this, $elementTypePtr value)"
    val prependHeader = st"void ${name}__prepend($name result, StackFrame caller, $name this, $elementTypePtr value)"
    val appendAllHeader = st"void ${name}__appendAll($name result, StackFrame caller, $name this, $name other)"
    val removeHeader = st"void ${name}__remove($name result, StackFrame caller, $name this, $elementTypePtr value)"
    val removeAllHeader = st"void ${name}__removeAll($name result, StackFrame caller, $name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string(String result, StackFrame caller, $name this)"
    val header =
      st"""// $tpe
      |$eqHeader;
      |$createHeader;
      |$zreateHeader;
      |$appendHeader;
      |$prependHeader;
      |$appendAllHeader;
      |$removeHeader;
      |$removeAllHeader;
      |$cprintHeader;
      |$stringHeader;
      |
      |static inline B ${name}__ne($name this, $name other) {
      |  return !${name}__eq(this, other);
      |}
      |$toOtherOpt"""
    val impl: ST =
      if (isElementTypeScalar)
        st"""// $tpe
        |
        |$eqHeader {
        |  $sizeType size = this->size;
        |  if (size != other->size) return F;
        |  for ($sizeType i = 0; i < size; i++) {
        |    if (${elementTypePtr}__ne(this->value[i], other->value[i])) return F;
        |  }
        |  return T;
        |}
        |
        |$createHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "create", 0);
        |  sfAssert(size <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType zize = ($sizeType) size;
        |  for ($sizeType i = 0; i < zize; i++) {
        |    result->value[i] = dflt;
        |  }
        |  result->size = zize;
        |}
        |
        |$zreateHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "zreate", 0);
        |  sfAssert(size <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType zize = ($sizeType) size;
        |  for ($sizeType i = 0; i < zize; i++) {
        |    result->value[i] = dflt;
        |  }
        |  result->size = zize;
        |}
        |
        |$appendHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
        |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType thisSize = this->size;
        |  Type_assign(result, this, sizeof(struct $name));
        |  result->value[thisSize] = value;
        |  result->size = ($sizeType) thisSize + 1;
        |}
        |
        |$prependHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
        |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType thisSize = this->size;
        |  result->value[0] = value;
        |  for ($sizeType i = 0; i < thisSize; i++)
        |    result->value[i + 1] = this->value[i];
        |  result->size = ($sizeType) thisSize + 1;
        |}
        |
        |$appendAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
        |  sfAssert(this->size + other->size <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType thisSize = this->size;
        |  $sizeType otherSize = other->size;
        |  Type_assign(result, this, sizeof($name));
        |  for ($sizeType i = 0; i < otherSize; i++)
        |    result->value[thisSize + i] = other->value[i];
        |  result->size = ($sizeType) thisSize + otherSize;
        |}
        |
        |$removeHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "-", 0);
        |  $sizeType thisSize = this->size;
        |  $sizeType k = 0;
        |  for ($sizeType i = 0; i < thisSize; i++) {
        |    $elementTypePtr o = this->value[i];
        |    if (${elementTypePtr}__ne(o, value)) result->value[k++] = o;
        |  }
        |  result->size = k;
        |}
        |
        |$removeAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "--", 0);
        |  $sizeType thisSize = this->size;
        |  $sizeType otherSize = other->size;
        |  $sizeType k = 0;
        |  for ($sizeType i = 0; i < thisSize; i++) {
        |    B found = F;
        |    $elementTypePtr o = this->value[i];
        |    for ($sizeType j = 0; j < otherSize && !found; j++)
        |      if (${elementTypePtr}__eq(o, other->value[j])) found = T;
        |    if (!found) result->value[k++] = o;
        |  }
        |  result->size = k;
        |}
        |
        |$cprintHeader {
        |  String_cprint(string("["), isOut);
        |  $sizeType size = this->size;
        |  if (size > 0) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_cprint(space, isOut);
        |    ${elementTypePtr}_cprint(value[0], isOut);
        |    for ($sizeType i = 1; i < size; i++) {
        |      String_cprint(string(", "), isOut);
        |      ${elementTypePtr}_cprint(value[i], isOut);
        |    }
        |    String_cprint(space, isOut);
        |  }
        |  String_cprint(string("]"), isOut);
        |}
        |
        |$stringHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
        |  String_string(result, sf, string("["));
        |  $sizeType size = this->size;
        |  if (size > 0) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_string(result, sf, space);
        |    ${elementTypePtr}_string(result, sf, value[0]);
        |    for ($sizeType i = 1; i < size; i++) {
        |      String_string(result, sf, string(", "));
        |      ${elementTypePtr}_string(result, sf, value[i]);
        |    }
        |    String_string(result, sf, space);
        |  }
        |  String_string(result, sf, string("]"));
        |}"""
      else
        st"""// $tpe
        |
        |$eqHeader {
        |  $sizeType size = this->size;
        |  if (size != other->size) return F;
        |  for ($sizeType i = 0; i < size; i++) {
        |    if (${elementTypePtr}__ne(($elementTypePtr) &this->value[i], ($elementTypePtr) &other->value[i])) return F;
        |  }
        |  return T;
        |}
        |
        |$createHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "create", 0);
        |  sfAssert(size <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType zize = ($sizeType) size;
        |  for ($sizeType i = 0; i < zize; i++) {
        |    Type_assign(&result->value[i], dflt, sizeof($elementType));
        |  }
        |  result->size = zize;
        |}
        |
        |$zreateHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "zreate", 0);
        |  sfAssert(size <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType zize = ($sizeType) size;
        |  for ($sizeType i = 0; i < zize; i++) {
        |    Type_assign(&result->value[i], dflt, sizeof($elementType));
        |  }
        |  result->size = zize;
        |}
        |
        |$appendHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
        |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType thisSize = this->size;
        |  $sizeType size = thisSize + 1;
        |  Type_assign(result, this, sizeof(struct $name));
        |  Type_assign(&result->value[thisSize], value, sizeof($elementType));
        |  result->size = size;
        |}
        |
        |$prependHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
        |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType thisSize = this->size;
        |  $sizeType size = thisSize + 1;
        |  Type_assign(&result->value[0], value, sizeof($elementType));
        |  for ($sizeType i = 0; i < thisSize; i++)
        |    Type_assign(&result->value[i + 1], &this->value[i], sizeof($elementType));
        |  result->size = size;
        |}
        |
        |$appendAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
        |  sfAssert(this->size + other->size <= Max$name, "Insufficient maximum for $tpe elements.");
        |  $sizeType thisSize = this->size;
        |  $sizeType otherSize = other->size;
        |  $sizeType size = thisSize + otherSize;
        |  Type_assign(result, this, sizeof(struct $name));
        |  for ($sizeType i = 0; i < otherSize; i++)
        |    Type_assign(&result->value[thisSize + i], &other->value[i], sizeof($elementType));
        |  result->size = size;
        |}
        |
        |$removeHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "-", 0);
        |  $sizeType thisSize = this->size;
        |  $sizeType k = 0;
        |  for ($sizeType i = 0; i < thisSize; i++) {
        |    $elementTypePtr o = ($elementTypePtr) &this->value[i];
        |    if (${elementTypePtr}__ne(o, value))
        |      Type_assign(&result->value[k++], o, sizeof($elementType));
        |  }
        |  result->size = k;
        |}
        |
        |$removeAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "--", 0);
        |  $sizeType thisSize = this->size;
        |  $sizeType otherSize = other->size;
        |  $sizeType k = 0;
        |  for ($sizeType i = 0; i < thisSize; i++) {
        |    B found = F;
        |    $elementTypePtr o = ($elementTypePtr) &this->value[i];
        |    for ($sizeType j = 0; j < otherSize && !found; j++)
        |      if (${elementTypePtr}__eq(o, ($elementTypePtr) &other->value[j])) found = T;
        |    if (!found) Type_assign(&result->value[k++], o, sizeof($elementType));
        |  }
        |  result->size = k;
        |}
        |
        |$cprintHeader {
        |  String_cprint(string("["), isOut);
        |  $sizeType size = this->size;
        |  if (size > 0) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_cprint(space, isOut);
        |    ${elementTypePtr}_cprint(&value[0], isOut);
        |    for ($sizeType i = 1; i < size; i++) {
        |      String_cprint(string(", "), isOut);
        |      ${elementTypePtr}_cprint(&value[i], isOut);
        |    }
        |    String_cprint(space, isOut);
        |  }
        |  String_cprint(string("]"), isOut);
        |}
        |
        |$stringHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
        |  String_string(result, sf, string("["));
        |  $sizeType size = this->size;
        |  if (size > 0) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_string(result, sf, space);
        |    ${elementTypePtr}_string(result, sf, ($elementTypePtr) &(value[0]));
        |    for ($sizeType i = 1; i < size; i++) {
        |      String_string(result, sf, string(", "));
        |      ${elementTypePtr}_string(result, sf, ($elementTypePtr) &(value[i]));
        |    }
        |    String_string(result, sf, space);
        |  }
        |  String_string(result, sf, string("]"));
        |}"""
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ header,
      impl = compiled.impl :+ impl
    )
  }

  @pure def elementName(owner: QName, id: String): ST = {
    return st"${mangleName(owner)}_${enumName(id)}"
  }

  @pure def enum(
    compiled: Compiled,
    uri: String,
    name: QName,
    elements: ISZ[String],
    optElementTypeOpt: Option[(ST, ST, ST)],
    iszElementTypeOpt: Option[ST]
  ): Compiled = {
    val mangledName = mangleName(name)

    @pure def enumCase(element: String): ST = {
      val r = st"""case ${mangledName}_${enumName(element)}: String_assign(result, string("$element")); return;"""
      return r
    }

    @pure def elementName(id: String): ST = {
      return st"${mangledName}_${enumName(id)}"
    }

    val indices = elements.indices.map((n: Z) => n)

    val tpe = dotName(name)

    val typeHeader =
      st"""// $tpe
      |typedef enum {
      |  ${(
        for (e <- ops.ISZOps(elements).zip(elements.indices.map(n => n)))
          yield st"${elementName(e._1)} = ${e._2}",
        ",\n"
      )}
      |} $mangledName;
      |
      |static inline B ${mangledName}__eq($mangledName this, $mangledName other) {
      |  return this == other;
      |}
      |
      |static inline B ${mangledName}__ne($mangledName this, $mangledName other) {
      |  return this != other;
      |}
      |
      |static inline Z ${mangledName}__ordinal($mangledName this) {
      |  return (Z) this;
      |}
      |
      |static inline void ${mangledName}_name(String result, $mangledName this) {
      |  switch (this) {
      |    ${(for (e <- elements) yield enumCase(e), "\n")}
      |  }
      |}"""

    var header: ISZ[ST] = ISZ()
    var impl: ISZ[ST] = ISZ()

    optElementTypeOpt match {
      case Some((optElementType, someElementType, noneElementType)) =>
        val byNameHeader = st"void ${mangledName}_byName($optElementType result, StackFrame caller, String s)"
        header = header :+ byNameHeader
        impl = impl :+
          st"""$byNameHeader {
          |  ${(
            for (e <- elements)
              yield
                st"""if (String__eq(s, string("$e"))) Type_assign(result, &((struct $someElementType) { .type = T$someElementType, .value = ${elementName(
                  e
                )} }), sizeof(union $optElementType));""",
            "\nelse "
          )}
          |  else Type_assign(result, &((struct $noneElementType) { .type = T$noneElementType }), sizeof(union $optElementType));
          |}"""

        val byOrdinalHeader = st"void ${mangledName}_byOrdinal($optElementType result, StackFrame caller, Z n)"
        header = header :+ byOrdinalHeader
        impl = impl :+
          st"""$byOrdinalHeader {
          |  switch (($mangledName) n) {
          |    ${(
            for (e <- elements)
              yield
                st"""case ${elementName(e)}: Type_assign(result, &((struct $someElementType) { .type = T$someElementType, .value = ${elementName(
                  e
                )} }), sizeof(union $optElementType)); return;""",
            "\n"
          )}
          |    default: Type_assign(result, &((struct $noneElementType) { .type = T$noneElementType }), sizeof(union $optElementType)); return;
          |  }
          |}"""
      case _ =>
    }

    iszElementTypeOpt match {
      case Some(iszElementType) =>
        val elementsHeader = st"void ${mangledName}_elements($iszElementType result)"
        header = header :+ elementsHeader
        impl = impl :+
          st"""$elementsHeader {
          |  result->size = Z_C(${elements.size});
          |  ${(
            for (p <- ops.ISZOps(elements).zip(indices)) yield st"result->value[${p._2}] = ${elementName(p._1)};",
            "\n"
          )}
          |}"""
      case _ =>
    }

    val numOfElementsHeader = st"Z ${mangledName}_numOfElements()"
    header = header :+ numOfElementsHeader
    impl = impl :+
      st"""$numOfElementsHeader {
      |  return Z_C(${elements.size});
      }"""

    val cprintHeader = st"void ${mangledName}_cprint($mangledName this, B isOut)"
    header = header :+ cprintHeader
    impl = impl :+
      st"""$cprintHeader {
      |  switch (this) {
      |    ${(
        for (e <- elements) yield st"""case ${elementName(e)}: String_cprint(string("$e"), isOut); return;""",
        "\n"
      )}
      |  }
      |}"""

    val stringHeader = st"void ${mangledName}_string(String result, StackFrame caller, $mangledName this)"
    header = header :+ stringHeader
    impl = impl :+
      st"""$stringHeader {
      |  DeclNewStackFrame(caller, "$uri", "$mangledName", "string", 0);
      |  switch (this) {
      |    ${(
        for (e <- elements) yield st"""case ${elementName(e)}: String_string(result, sf, string("$e")); return;""",
        "\n"
      )}
      |  }
      |}"""
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+
        st"""// $tpe
        |${(header, ";\n")};""",
      impl = compiled.impl :+
        st"""// $tpe
        |
        |${(impl, "\n\n")}"""
    )
  }

  @pure def subz(
    compiled: Compiled,
    uri: String,
    name: QName,
    bitWidth: Z,
    isBitVector: B,
    isUnsigned: B,
    minOpt: Option[Z],
    maxOpt: Option[Z],
    optionTypeOpt: Option[(ST, ST, ST)]
  ): Compiled = {
    val mangledName = mangleName(name)
    val cType = st"${if (isUnsigned) "u" else ""}int${bitWidth}_t"
    val cTypeUp = st"${if (isUnsigned) "U" else ""}INT$bitWidth"
    val pr = st"PRI${if (isUnsigned) if (isBitVector) "x" else "u" else "d"}$bitWidth"
    val shift: ST = if (!isBitVector) { st"" } else if (isUnsigned) {
      st"""
      |static inline $mangledName ${mangledName}__complement($mangledName n) {
      |  return ~n;
      |}
      |
      |static inline $mangledName ${mangledName}__shl($mangledName n1, $mangledName n2) {
      |  return n1 << n2;
      |}
      |
      |static inline $mangledName ${mangledName}__shr($mangledName n1, $mangledName n2) {
      |  return n1 >> n2;
      |}
      |
      |static inline $mangledName ${mangledName}__ushr($mangledName n1, $mangledName n2) {
      |  return n1 >> n2;
      |}
      |
      |static inline $mangledName ${mangledName}__and($mangledName n1, $mangledName n2) {
      |  return n1 & n2;
      |}
      |
      |static inline $mangledName ${mangledName}__or($mangledName n1, $mangledName n2) {
      |  return n1 | n2;
      |}
      |
      |static inline $mangledName ${mangledName}__xor($mangledName n1, $mangledName n2) {
      |  return n1 ^ n2;
      |}"""
    } else {
      val unsigned: String = bitWidth match {
        case z"8" => "uint8_t"
        case z"16" => "uint16_t"
        case z"32" => "uint32_t"
        case z"64" => "uint64_t"
      }
      st"""
      |static inline $mangledName ${mangledName}__complement($mangledName n) {
      |  $unsigned un = ($unsigned) n;
      |  return ($mangledName) ~un;
      |}
      |
      |static inline $mangledName ${mangledName}__shl($mangledName n1, $mangledName n2) {
      |  $unsigned un1 = ($unsigned) n1;
      |  $unsigned un2 = ($unsigned) n2;
      |  return ($mangledName) (un1 << un2);
      |}
      |
      |static inline $mangledName ${mangledName}__shr($mangledName n1, $mangledName n2) {
      |  $unsigned un1 = ($unsigned) n1;
      |  $unsigned un2 = ($unsigned) n2;
      |  return ($mangledName) (un1 >> un2);
      |}
      |
      |static inline $mangledName ${mangledName}__ushr($mangledName n1, $mangledName n2) {
      |  $unsigned un1 = ($unsigned) n1;
      |  $unsigned un2 = ($unsigned) n2;
      |  return ($mangledName) (un1 >> un2);
      |}
      |
      |static inline $mangledName ${mangledName}__and($mangledName n1, $mangledName n2) {
      |  $unsigned un1 = ($unsigned) n1;
      |  $unsigned un2 = ($unsigned) n2;
      |  return ($mangledName) (un1 & un2);
      |}
      |
      |static inline $mangledName ${mangledName}__or($mangledName n1, $mangledName n2) {
      |  $unsigned un1 = ($unsigned) n1;
      |  $unsigned un2 = ($unsigned) n2;
      |  return ($mangledName) (un1 | un2);
      |}
      |
      |static inline $mangledName ${mangledName}__xor($mangledName n1, $mangledName n2) {
      |  $unsigned un1 = ($unsigned) n1;
      |  $unsigned un2 = ($unsigned) n2;
      |  return ($mangledName) (un1 ^ un2);
      |}"""
    }
    val min: ST = minOpt match {
      case Some(m) => st"${mangledName}_C($m)"
      case _ => st"${mangledName}_C(${cTypeUp}_MIN)"
    }
    val max: ST = maxOpt match {
      case Some(m) => st"${mangledName}_C($m)"
      case _ => st"${mangledName}_C(${cTypeUp}_MAX)"
    }
    val hex: ST = if (isBitVector && isUnsigned) st"0${bitWidth / 4}" else st""
    val typeHeader =
      st"""typedef $cType $mangledName;
      |
      |#define ${mangledName}_C(n) ${cTypeUp}_C(n)
      |
      |#define ${mangledName}_Min ${cTypeUp}_MIN
      |#define ${mangledName}_Max ${cTypeUp}_MAX
      |
      |#define ${mangledName}_F "%$hex" $pr ""
      |
      |#define ${mangledName}__plus(n) n
      |
      |static inline $mangledName ${mangledName}__minus($mangledName n) {
      |  return ($mangledName) -n;
      |}
      |
      |static inline $mangledName ${mangledName}__add($mangledName n1, $mangledName n2) {
      |  return ($mangledName) (n1 + n2);
      |}
      |
      |static inline $mangledName ${mangledName}__sub($mangledName n1, $mangledName n2) {
      |  return ($mangledName) (n1 - n2);
      |}
      |
      |static inline $mangledName ${mangledName}__mul($mangledName n1, $mangledName n2) {
      |  return ($mangledName) (n1 * n2);
      |}
      |
      |static inline $mangledName ${mangledName}__div($mangledName n1, $mangledName n2) {
      |  return ($mangledName) (n1 / n2);
      |}
      |
      |static inline $mangledName ${mangledName}__rem($mangledName n1, $mangledName n2) {
      |  return ($mangledName) (n1 % n2);
      |}
      |
      |static inline B ${mangledName}__eq($mangledName n1, $mangledName n2) {
      |  return (B) (n1 == n2);
      |}
      |
      |static inline B ${mangledName}__ne($mangledName n1, $mangledName n2) {
      |  return (B) (n1 != n2);
      |}
      |
      |static inline B ${mangledName}__lt($mangledName n1, $mangledName n2) {
      |  return (B) (n1 < n2);
      |}
      |
      |static inline B ${mangledName}__le($mangledName n1, $mangledName n2) {
      |  return (B) (n1 == n2);
      |}
      |
      |static inline B ${mangledName}__gt($mangledName n1, $mangledName n2) {
      |  return (B) (n1 > n2);
      |}
      |
      |static inline B ${mangledName}__ge($mangledName n1, $mangledName n2) {
      |  return (B) (n1 >= n2);
      |}
      |$shift"""
    val stringHeader = st"void ${mangledName}_string(String result, StackFrame caller, $mangledName this)"

    var header = ISZ(
      st"#define ${mangledName}_cprint(this, isOut) { if (isOut) printf(${mangledName}_F, this); else fprintf(stderr, ${mangledName}_F, this); }",
      stringHeader
    )
    var impl = ISZ(st"""$stringHeader {
    |  DeclNewStackFrame(caller, "$uri", "${dotName(name)}", "string", 0);
    |  int nSize = sprintf(NULL, 0, ${mangledName}_F, this);
    |  Z size = result->size;
    |  Z newSize = size + nSize;
    |  sfAssert(newSize <= MaxString, "Insufficient maximum for String characters.");
    |  snprintf(&(result->value[result->size]), nSize, ${mangledName}_F, this);
    |  result->size = newSize;
    |}""")
    optionTypeOpt match {
      case Some((optionName, someName, noneName)) =>
        val (ct, strTo): (String, String) =
          if (isUnsigned) ("unsigned long long", "strtoull") else ("long long", "strtoll")
        val p = strToNum(name, optionName, someName, noneName, ct, strTo, T, T, min, max)
        header = header :+ p._1
        impl = impl :+ p._2
      case _ =>
    }
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ st"${(header, ";\n")};",
      impl = compiled.impl :+ st"${(impl, "\n\n")}"
    )
  }

  @pure def strToNum(
    name: QName,
    optName: ST,
    someName: ST,
    noneName: ST,
    cType: String,
    cStrTo: String,
    hasBase: B,
    hasRange: B,
    min: ST,
    max: ST
  ): (ST, ST) = {
    val mangledName = mangleName(name)
    val header = st"void ${mangledName}_apply($optName result, StackFrame sf, String s)"
    val base: ST = if (hasBase) st", 0" else st""
    val rangeCheck: ST = if (hasRange) st"" else st" && $min <= n && n <= $max"
    val impl =
      st"""$header {
      |  char *endptr;
      |  errno = 0;
      |  $cType n = $cStrTo(s->value, &endptr$base);
      |  if (errno) {
      |    errno = 0;
      |    Type_assign(result, &((struct $noneName) { .type = T$noneName }));
      |    return;
      |  }
      |  if (s->value - endptr == 0$rangeCheck)
      |    Type_assign(result, &((struct $someName) { .type = T$someName, .value = ($mangledName) n }));
      |  else Type_assign(result, &((struct $noneName) { .type = T$noneName }));
      |}"""
    return (header, impl)
  }

  @pure def strToB(optName: ST, someName: ST, noneName: ST): (ST, ST) = {
    val header = st"void B_apply($optName result, String s)"
    val impl =
      st"""$header {
      |  B r;
      |  B noResult = F;
      |  if (s->size == 1)
      |    if (s->value[0] == 'T') r = T;
      |    else if (s->value[0] == 'F') r = F;
      |    else noResult = T;
      |  else if (s->size == 4 && memcmp(s->value, "true", 4) == 0) r = T;
      |  else if (s->size == 5 && memcmp(s->value, "false", 5) == 0) r = F;
      |  else noResult = T;
      |  if (noResult) Type_assign(result, &((struct $noneName) { .type = T$noneName }))
      |  else if (r) Type_assign(result, &((struct $someName) { .type = T$someName, .value = T }));
      |  else Type_assign(result, &((struct $someName) { .type = T$someName, .value = F }));
      |}"""
    return (header, impl)
  }

  @pure def tupleName(size: Z): QName = {
    return AST.Typed.sireumName :+ s"Tuple$size"
  }

  @pure def funName(size: Z): QName = {
    return AST.Typed.sireumName :+ s"Fun$size"
  }

  @pure def tuple(
    compiled: Compiled,
    tpe: String,
    includes: ISZ[ST],
    name: ST,
    constructorParamTypes: ISZ[(TypeKind.Type, ST, ST)]
  ): Compiled = {
    val indices = constructorParamTypes.indices.map((n: Z) => n)
    var members = ISZ[ST]()
    for (p <- ops
        .ISZOps(ops.ISZOps(constructorParamTypes).zip(indices))
        .sortWith((p1, p2) => p1._1._1.ordinal < p2._1._1.ordinal)) {
      val ((_, t, _), i) = p
      val index = i + 1
      members = members :+ st"$t _$index;"
    }
    val size = constructorParamTypes.size
    val typeHeader =
      st"""// $tpe
      |${(includes, "\n")}
      |
      |typedef struct $name *$name;
      |struct $name {
      |  TYPE type;
      |  ${(members, "\n")}
      |};
      |
      |#define DeclNew$name(x) struct $name x = { .type = T$name }
      |#define ${name}_size(this) Z_C($size)"""
    val cParamTypes: ISZ[ST] = for (p <- constructorParamTypes) yield p._3
    val cParams: ISZ[ST] = for (p <- ops.ISZOps(cParamTypes).zip(indices)) yield st"${p._1} _${p._2 + 1}"
    val constructorHeader = st"void ${name}_apply(StackFrame caller, $name this, ${(cParams, ", ")})"
    val typesIndices: ISZ[((TypeKind.Type, ST, ST), Z)] = ops.ISZOps(constructorParamTypes).zip(indices)
    var accessors = ISZ[ST]()
    var constructorStmts = ISZ[ST]()
    for (p <- typesIndices) {
      val ((kind, _, tPtr), i) = p
      val index = i + 1
      if (isScalar(kind)) {
        accessors = accessors :+
          st"""static inline $tPtr ${name}_$index($name this) {
          |  return this->_$index;
          |}"""
        constructorStmts = constructorStmts :+ st"this->_$index = _$index;"
      } else {
        val us: String = if (isTrait(kind)) "union" else "struct"
        accessors = accessors :+
          st"""static inline $tPtr ${name}_$index($name this) {
          |  return ($tPtr) &this->_$index;
          |}"""
        constructorStmts = constructorStmts :+
          st"Type_assign(&(this->_$index), _$index, sizeof($us $tPtr));"
      }
    }
    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string(String result, StackFrame caller, $name this)"
    val header =
      st"""// $tpe
      |$constructorHeader;
      |$eqHeader;
      |$cprintHeader;
      |$stringHeader;
      |
      |${(accessors, "\n\n")};
      |
      |static inline B ${name}__ne($name this, $name other) {
      |  return !${name}__eq(this, other);
      |}"""
    var eqStmts = ISZ[ST]()
    for (i <- cParamTypes.indices) {
      val t = cParamTypes(i)
      val amp: ST = if (isScalar(constructorParamTypes(i)._1)) st"" else st"($t) &"
      eqStmts = eqStmts :+ st"if (${t}__ne(${amp}this->_${i + 1}, ${amp}other->_${i + 1})) return F;"
    }
    val impl =
      st"""// $tpe
      |
      |$constructorHeader {
      |  DeclNewStackFrame(caller, "Tuple$size.scala", "${dotName(tupleName(size))}", "apply", 0);
      |  ${(constructorStmts, "\n")}
      |}
      |
      |$eqHeader {
      |  ${(eqStmts, "\n")}
      |  return T;
      |}
      |
      |$cprintHeader {
      |  String sep = string(", ");
      |  String_cprint(string("("), isOut);
      |  ${cParamTypes(0)}_cprint(${if (isScalar(constructorParamTypes(0)._1)) "" else "&"}this->_1, isOut);
      |  ${(
        for (i <- z"1" until size) yield st"""String_cprint(sep, isOut);
        |${cParamTypes(i)}_cprint(${if (isScalar(constructorParamTypes(i)._1)) "" else "&"}this->_${i + 1}, isOut);""",
        "\n"
      )}
      |  String_cprint(string(")"), isOut);
      |}
      |
      |$stringHeader {
      |  DeclNewStackFrame(caller, "Tuple$size.scala", "org.sireum.Tuple$size", "string", 0);
      |  String sep = string(", ");
      |  String_string(result, sf, string("("));
      |  ${cParamTypes(0)}_string(result, sf, ${if (isScalar(constructorParamTypes(0)._1)) ""
      else s"(${cParamTypes(0).render}) &"}this->_1);
      |  ${(
        for (i <- z"1" until size)
          yield
            st"""String_string(result, sf, sep);
            |${cParamTypes(i)}_string(result, sf, ${if (isScalar(constructorParamTypes(i)._1)) ""
            else s"(${cParamTypes(i).render}) &"}this->_${i + 1});""",
        "\n"
      )}
      |  String_string(result, sf, string(")"));
      |}"""
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ header,
      impl = compiled.impl :+ impl
    )
  }

  def obj(
    compiled: Compiled,
    uri: String,
    objectName: QName,
    name: ST,
    vars: ISZ[(TypeKind.Type, String, ST, ST, B)],
    initStmts: ISZ[ST]
  ): Compiled = {
    var accessorHeaders = ISZ[ST]()
    var accessors = ISZ[ST]()
    var globals = ISZ[ST]()
    for (q <- vars) {
      val (kind, id, t, tPtr, isVar) = q
      val h = st"$tPtr ${name}_$id(StackFrame caller)"
      globals = globals :+ st"$t _${name}_$id;"
      accessorHeaders = accessorHeaders :+ st"$h;"
      val scalar = isScalar(kind)
      accessors = accessors :+
        st"""$h {
        |  ${name}_init(caller);
        |  return ${if (scalar) "" else "&"}_${name}_$id;
        |}"""
      if (isVar) {
        val h2 = st"void ${name}_${id}_a(StackFrame caller, $tPtr p_$id)"
        accessorHeaders = accessorHeaders :+ st"$h2;"
        if (scalar) {
          accessors = accessors :+
            st"""$h2 {
            |  ${name}_init(caller);
            |  _${name}_$id = p_$id;
            |}"""
        } else {
          accessors = accessors :+
            st"""$h2 {
            |  ${name}_init(caller);
            |  Type_assign(&_${name}_$id, p_$id, sizeof($t));
            |}"""
        }
      }
    }
    val header =
      st"""void ${name}_init(StackFrame caller);
      |
      |${(accessorHeaders, "\n")}"""
    val impl =
      st"""B ${name}_initialized_ = F;
      |
      |${(globals, "\n")}
      |
      |void ${name}_init(StackFrame caller) {
      |  if (${name}_initialized_) return;
      |  ${name}_initialized_ = T;
      |  DeclNewStackFrame(caller, "$uri", "${dotName(objectName)}", "<init>", 0);
      |  ${(initStmts, "\n")}
      |}
      |
      |${(accessors, "\n\n")}"""
    return compiled(header = header +: compiled.header, impl = impl +: compiled.impl)
  }

  @pure def commaArgs(args: ISZ[ST]): ST = {
    return if (args.isEmpty) st"" else st", ${(args, ", ")}"
  }

  @pure def dotName(ids: QName): String = {
    return st"${(ids, ".")}".render
  }

  @pure def localName(id: String): ST = {
    return if (keywords.contains(id)) st"l_$id" else encodeName(id)
  }

  @pure def fieldName(id: String): ST = {
    return if (keywords.contains(id)) st"f_$id" else encodeName(id)
  }

  @pure def enumName(id: String): ST = {
    return if (keywords.contains(id)) st"E_$id" else encodeName(id)
  }

  @pure def mangleName(ids: QName): ST = {
    val r: ST =
      if (ids.size == z"1") st"top_${ids(0)}"
      else st"${(AST.Typed.short(ids).map(encodeName), "_")}"
    return r
  }

  @pure def encodeName(id: String): ST = {
    return st"$id" // TODO
  }

  @pure def typeName(t: AST.Typed): QName = {
    return t.asInstanceOf[AST.Typed.Name].ids
  }

  @pure def removeExt(filename: String): String = {
    val sops = ops.StringOps(filename)
    val i = sops.lastIndexOf('.')
    if (i < 0) {
      return filename
    } else {
      return sops.substring(0, i)
    }
  }

  @pure def filename(uriOpt: Option[String], default: String): String = {
    uriOpt match {
      case Some(uri) =>
        val sops = ops.StringOps(uri)
        val i = sops.lastIndexOf('/')
        if (i < 0) {
          return uri
        }
        return sops.substring(i + 1, uri.size)
      case _ => return default
    }
  }

  @pure def filenameOfPosOpt(posOpt: Option[Position], default: String): String = {
    posOpt match {
      case Some(pos) => return filename(pos.uriOpt, default)
      case _ => return default
    }
  }

  @pure def isTrait(kind: TypeKind.Type): B = {
    kind match {
      case TypeKind.ImmutableTrait =>
      case TypeKind.MutableTrait =>
      case _ => return F
    }
    return T
  }

  @pure def isImmutable(kind: TypeKind.Type): B = {
    kind match {
      case TypeKind.Scalar1 =>
      case TypeKind.Scalar8 =>
      case TypeKind.Scalar16 =>
      case TypeKind.Scalar32 =>
      case TypeKind.Scalar64 =>
      case TypeKind.R =>
      case TypeKind.Enum =>
      case TypeKind.IS =>
      case TypeKind.Immutable =>
      case TypeKind.ImmutableTrait =>
      case _ => return F
    }
    return T
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

  @pure def isClass(kind: TypeKind.Type): B = {
    kind match {
      case TypeKind.Immutable =>
      case TypeKind.Mutable =>
      case TypeKind.IS =>
      case TypeKind.MS =>
      case _ => return F
    }
    return T
  }

  @pure def typePrefix(kind: TypeKind.Type): String = {
    return if (isScalar(kind)) "" else if (isTrait(kind)) "union " else "struct "
  }
}
