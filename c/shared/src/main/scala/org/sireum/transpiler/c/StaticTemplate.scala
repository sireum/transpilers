// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol.Resolver._
import org.sireum.message._

object StaticTemplate {

  @datatype class Compiled(typeHeader: ISZ[ST], header: ISZ[ST], impl: ISZ[ST])

  val sireumDir: String = "sireum"
  val libraryDir: String = "library"

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

  @pure def typeCompositeH(stringMax: Z, isStringMax: Z, typeNames: ISZ[(String, ST)]): ST = {
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
      |static inline B String_eq(String this, String other) {
      |  Z thisSize = this->size;
      |  if (thisSize != other->size) return F;
      |  return memcmp(this->value, other->value, (size_t) thisSize) == 0;
      |}
      |
      |static inline B String_ne(String this, String other) {
      |  return !String_eq(this, other);
      |}
      |
      |#endif"""
    return r
  }

  @pure def typesH(names: ISZ[QName], typeNames: ISZ[(String, ST)]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_H
      |#define SIREUM_GEN_H
      |
      |#include <misc.h>
      |${(for (name <- ops.ISZOps(names).sortWith(qnameLt)) yield st"#include <type-${(name, "_")}.h>", "\n")}
      |
      |static inline size_t sizeOf(Type t) {
      |  switch (t->type) {
      |    ${(for (tn <- typeNames) yield st"case T${tn._2}: return sizeof(struct ${tn._2}); // ${tn._1}", "\n")}
      |  }
      |}
      |
      |void Type_assign(void *dest, void *src, size_t destSize);
      |#endif"""
    return r
  }

  @pure def typesC(): ST = {
    val r =
      st"""#include <types.h>
      |
      |void Type_assign(void *dest, void *src, size_t destSize) {
      |  size_t srcSize = sizeOf((Type) src);
      |  memcpy(dest, src, srcSize);
      |  memset(((char *) dest) + srcSize, 0, destSize - srcSize);
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
      |#endif"""
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

  @pure def main(filename: String, owner: QName, id: String): ST = {
    val r =
      st"""#include <all.h>
      |
      |int main(int argc, char *argv[]) {
      |  DeclNewStackFrame(NULL, "$filename", "${dotName(owner)}", "<main>", 0);
      |
      |  DeclNewAString(t_args);
      |  AString args = (AString) &t_args;
      |
      |  int size = argc - 1;
      |  if (size > MaxAString) {
      |    sfAbort("Insufficient maximum for String elements.");
      |  }
      |
      |  for (int i = 0; i < size; i++) {
      |    char *arg = argv[i + 1];
      |    size_t argSize = strlen(arg);
      |    if (argSize > MaxString) {
      |      sfAbort("Insufficient maximum for String characters.");
      |    }
      |    AString_at(args, i)->size = (Z) argSize;
      |    memcpy(AString_at(args, i)->value, arg, argSize + 1);
      |  }
      |
      |  AString_size(args) = size;
      |
      |  ${mangleName(owner :+ id)}(args);
      |}"""
    return r
  }

  @pure def array(
    compiled: Compiled,
    includes: ISZ[ST],
    tpe: String,
    isImmutable: B,
    name: ST,
    indexType: ST,
    minIndex: Z,
    isIndexTypeScalar: B,
    elementType: ST,
    elementTypePtr: ST,
    maxElement: Z
  ): Compiled = {
    val offset: ST = if (minIndex == z"0") st"" else st"- ${indexType}_C($minIndex)"
    val sName: String = if (isImmutable) "IS" else "MS"
    val typeHeader =
      st"""// $tpe
      |${(includes, "\n")}
      |
      |#ifndef Max$name
      |#define Max$name $maxElement
      |#endif
      |
      |typedef struct $name *$name;
      |struct $name {
      |  TYPE type;
      |  $indexType size;
      |  $elementType value[Max$name];
      |};
      |
      |#define DeclNew$name(x) struct $name x = { .type = T$name }
      |#define ${name}_size(this) ((this)->size)
      |#define ${name}_at(this, i) (($elementTypePtr) &((this)->value[i$offset]))"""
    val appendHeader = st"void ${name}_append($name result, StackFrame caller, $name this, $elementTypePtr value)"
    val prependHeader = st"void ${name}_prepend($name result, StackFrame caller, $name this, $elementTypePtr value)"
    val appendAllHeader = st"void ${name}_appendAll($name result, StackFrame caller, $name this, $name other)"
    val removeHeader = st"void ${name}_remove($name result, StackFrame caller, $name this, $elementTypePtr value)"
    val removeAllHeader = st"void ${name}_removeAll($name result, StackFrame caller, $name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string(String result, StackFrame caller, $name this)"
    val header =
      st"""// $tpe
      |$appendHeader;
      |$prependHeader;
      |$appendAllHeader;
      |$removeHeader;
      |$removeAllHeader;
      |$cprintHeader;
      |$stringHeader;"""
    val impl: ST =
      if (isIndexTypeScalar)
        st""" // $tpe
        |
        |$appendHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
        |  $indexType thisSize = this->size;
        |  $indexType size = thisSize + 1;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(result, this, sizeof(struct $name));
        |  result->value[thisSize] = value;
        |  result->size = size;
        |}
        |
        |$prependHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
        |  $indexType thisSize = this->size;
        |  $indexType size = thisSize + 1;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  result->value[0] = value;
        |  for ($indexType i = ${indexType}_C(0); i < thisSize; i++)
        |    result->value[i + 1] = this->value[i];
        |  result->size = size;
        |}
        |
        |$appendAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
        |  $indexType thisSize = this->size;
        |  $indexType otherSize = other->size;
        |  $indexType size = thisSize + otherSize;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(result, this, sizeof($name));
        |  for ($indexType i = ${indexType}_C(0); i < otherSize; i++)
        |    result->value[thisSize + i] = other->value[i];
        |  result->size = size;
        |}
        |
        |$removeHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "-", 0);
        |  $indexType thisSize = this->size;
        |  $indexType k = ${indexType}_C(0);
        |  for ($indexType i = ${indexType}_C(0); i < thisSize; i++) {
        |    $elementTypePtr o = this->value[i];
        |    if (${elementTypePtr}_ne(o, value)) result->value[k++] = o;
        |  }
        |  result->size = k;
        |}
        |
        |$removeAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "--", 0);
        |  $indexType thisSize = this->size;
        |  $indexType otherSize = other->size;
        |  $indexType k = ${indexType}_C(0);
        |  for ($indexType i = ${indexType}_C(0); i < thisSize; i++) {
        |    B found = F;
        |    $elementTypePtr o = this->value[i];
        |    for ($indexType j = ${indexType}_C(0); j < otherSize && !found; j++)
        |      if (${elementTypePtr}_eq(o, other->value[j])) found = T;
        |    if (!found) result->value[k++] = o;
        |  }
        |  result->size = k;
        |}
        |
        |$cprintHeader {
        |  String_cprint(string("["), isOut);
        |  $indexType size = this->size;
        |  if (size > ${indexType}_C(0)) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_cprint(space, isOut);
        |    ${elementTypePtr}_cprint(value[0], isOut);
        |    for ($indexType i = ${indexType}_C(1); i < size; i++) {
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
        |  $indexType size = this->size;
        |  if (size > ${indexType}_C(0)) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_string(result, sf, space);
        |    ${elementTypePtr}_string(result, sf, value[0]);
        |    for ($indexType i = ${indexType}_C(1); i < size; i++) {
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
        |$appendHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
        |  $indexType thisSize = this->size;
        |  $indexType size = thisSize + 1;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(result, this, sizeof(struct $name));
        |  Type_assign(&result->value[thisSize], value, sizeof($elementType));
        |  result->size = size;
        |}
        |
        |$prependHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
        |  $indexType thisSize = this->size;
        |  $indexType size = thisSize + 1;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(&result->value[0], value, sizeof($elementType));
        |  for ($indexType i = ${indexType}_C(0); i < thisSize; i++)
        |    Type_assign(&result->value[i + 1], &this->value[i], sizeof($elementType));
        |  result->size = size;
        |}
        |
        |$appendAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
        |  $indexType thisSize = this->size;
        |  $indexType otherSize = other->size;
        |  $indexType size = thisSize + otherSize;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(result, this, sizeof(struct $name));
        |  for ($indexType i = ${indexType}_C(0); i < otherSize; i++)
        |    Type_assign(&result->value[thisSize + i], &other->value[i], sizeof($elementType));
        |  result->size = size;
        |}
        |
        |$removeHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "-", 0);
        |  $indexType thisSize = this->size;
        |  $indexType k = ${indexType}_C(0);
        |  for ($indexType i = ${indexType}_C(0); i < thisSize; i++) {
        |    $elementTypePtr o = &this->value[i];
        |    if (${elementTypePtr}_ne(o, value))
        |      Type_assign(&result->value[k++], o, sizeof($elementType));
        |  }
        |  result->size = k;
        |}
        |
        |$removeAllHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "--", 0);
        |  $indexType thisSize = this->size;
        |  $indexType otherSize = other->size;
        |  $indexType k = ${indexType}_C(0);
        |  for ($indexType i = ${indexType}_C(0); i < thisSize; i++) {
        |    B found = F;
        |    $elementTypePtr o = &this->value[i];
        |    for ($indexType j = ${indexType}_C(0); j < otherSize && !found; j++)
        |      if (${elementTypePtr}_eq(o, &other->value[j])) found = T;
        |    if (!found) Type_assign(&result->value[k++], o, sizeof($elementType));
        |  }
        |  result->size = k;
        |}
        |
        |$cprintHeader {
        |  String_cprint(string("["), isOut);
        |  $indexType size = this->size;
        |  if (size > ${indexType}_C(0)) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_cprint(space, isOut);
        |    ${elementTypePtr}_cprint(&(value[0]), isOut);
        |    for ($indexType i = ${indexType}_C(1); i < size; i++) {
        |      String_cprint(string(", "), isOut);
        |      ${elementTypePtr}_cprint(&(value[i]), isOut);
        |    }
        |    String_cprint(space, isOut);
        |  }
        |  String_cprint(string("]"), isOut);
        |}
        |
        |$stringHeader {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
        |  String_string(result, sf, string("["));
        |  $indexType size = this->size;
        |  if (size > ${indexType}_C(0)) {
        |    $elementType *value = this->value;
        |    String space = string(" ");
        |    String_string(result, sf, space);
        |    ${elementTypePtr}_string(result, sf, (&(value[0])));
        |    for ($indexType i = ${indexType}_C(1); i < size; i++) {
        |      String_string(result, sf, string(", "));
        |      ${elementTypePtr}_string(result, sf, &(value[i]));
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
      |static inline B ${mangledName}_eq($mangledName this, $mangledName other) {
      |  return this == other;
      |}
      |
      |static inline B ${mangledName}_ne($mangledName this, $mangledName other) {
      |  return this != other;
      |}
      |
      |static inline Z ${mangledName}_ordinal($mangledName this) {
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
        val byNameHeader = st"void ${mangledName}_byName($optElementType result, String s)"
        header = header :+ byNameHeader
        impl = impl :+
          st"""$byNameHeader {
          |  ${(
            for (e <- elements)
              yield
                st"""if (String_eq(s, string("$e"))) Type_assign(result, &((struct $someElementType) { .type = T$someElementType, .value = ${elementName(
                  e
                )} }), sizeof(union $optElementType));""",
            "\nelse"
          )}
          |  else Type_assign(result, &((struct $noneElementType) { .type = T$noneElementType }), sizeof(union $optElementType));
          |}"""

        val byOrdinalHeader = st"void ${mangledName}_byName($optElementType result, Z n)"
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

  @pure def strToNum(name: QName, optName: ST, someName: ST, noneName: ST, cType: String, cStrTo: String): (ST, ST) = {
    val mangledName = mangleName(name)
    val header = st"void ${mangledName}_apply($optName result, String s)"
    val impl =
      st"""$header {
      |  char *endptr;
      |  errno = 0;
      |  $cType n = $cStrTo(s->value, &endptr, 0);
      |  if (errno) {
      |    errno = 0;
      |    Type_assign(result, &((struct $noneName) { .type = T$noneName }));
      |    return;
      |  }
      |  if (s->value - endptr == 0) Type_assign(result, &((struct $someName) { .type = T$someName, .value = ($mangledName) n }));
      |  else Type_assign(result, &((struct $noneName) { .type = T$noneName }));
      |}"""
    return (header, impl)
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
      else if (ids.size >= 2 && ids(0) == string"org" && ids(1) == string"sireum")
        st"${(ops.ISZOps(ids).drop(2).map(encodeName), "_")}"
      else st"${(ids.map(encodeName), "_")}"
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
}
