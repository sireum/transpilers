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
  val worksheetFilename: ST = st"worksheet"

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
      |  T${typeNames(0)._2}, // ${typeNames(0)._1}
      |  ${(for (tn <- ops.ISZOps(typeNames).drop(1)) yield st"T${tn._2}, // ${tn._1}", "\n")}
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
      |#define DeclNewString(x) struct StaticString x; memset(&(x), 0, sizeof(struct StaticString)); (x).type = TString
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
    return if (name.size == z"1") worksheetFilename else st"${(name, "_")}"
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
  ): (ST, ST) = {
    val offset: ST = if (minIndex == z"0") st"" else st"- ${indexType}_C($minIndex)"
    val sName: String = if (isImmutable) "IS" else "MS"
    val header: ST =
      if (isIndexTypeScalar)
        st""" // $tpe
        |
        |static inline void ${name}_append(StackFrame caller, $name this, $elementTypePtr value, $name result) {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
        |  $indexType thisSize = this->size;
        |  $indexType size = thisSize + 1;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(result, this, sizeof(struct $name));
        |  result->value[thisSize] = value;
        |  result->size = size;
        |}
        |
        |static inline void ${name}_prepend(StackFrame caller, $name this, $elementTypePtr value, $name result) {
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
        |static inline void ${name}_appendAll(StackFrame caller, $name this, $name other, $name result) {
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
        |static inline void ${name}_remove(StackFrame caller, $name this, $elementTypePtr value, $name result) {
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
        |static inline void ${name}_removeAll(StackFrame caller, $name this, $name other, $name result) {
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
        |}"""
      else
        st""" // $tpe
        |
        |static inline void ${name}_append(StackFrame caller, $name this, $elementTypePtr value, $name result) {
        |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
        |  $indexType thisSize = this->size;
        |  $indexType size = thisSize + 1;
        |  sfAssert(!(${indexType}_C(0) <= size && size <= Max$name), "Insufficient maximum for $tpe elements.");
        |  Type_assign(result, this, sizeof(struct $name));
        |  Type_assign(&result->value[thisSize], value, sizeof($elementType));
        |  result->size = size;
        |}
        |
        |static inline void ${name}_prepend(StackFrame caller, $name this, $elementTypePtr value, $name result) {
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
        |static inline void ${name}_appendAll(StackFrame caller, $name this, $name other, $name result) {
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
        |static inline void ${name}_remove(StackFrame caller, $name this, $elementTypePtr value, $name result) {
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
        |static inline void ${name}_removeAll(StackFrame caller, $name this, $name other, $name result) {
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
        |}"""

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
      |#define DeclNew$name(x) struct $name; memset(&$name, 0, sizeof(struct $name)); $name.type = T$name
      |#define ${name}_size(this) ((this)->size)
      |#define ${name}_at(this, i) (($elementTypePtr) &((this)->value[i$offset]))"""
    return (typeHeader, header)
  }

  @pure def enum(tpe: String, name: ST, elements: ISZ[String]): ST = {
    @pure def enumCase(element: String): ST = {
      val r = st"""case ${name}_$element: String_assign(r, string("$element")); break;"""
      return r
    }

    val r =
      st"""// $tpe
      |typedef enum {
      |  ${(for (e <- elements) yield st"${name}_$e", ",\n")}
      |} $name;
      |
      |static inline Z ${name}_ordinal($name this) {
      |  return (Z) this;
      |}
      |
      |static inline struct StaticString ${name}_name($name this) {
      |  DeclNewString(r);
      |  switch (this) {
      |    ${(for (e <- elements) yield enumCase(e), "\n")}
      |  }
      |  return r;
      |}"""
    return r
  }

  @pure def dotName(ids: QName): String = {
    return st"${(ids, ".")}".render
  }

  @pure def localName(id: String): ST = {
    return st"l_${encodeName(id)}"
  }

  @pure def mangleName(ids: QName): ST = {
    val r: ST =
      if (ids.size == z"1") st"top_${ids(0)}"
      else if (ids.size >= 2 && ids(0) == string"org" && ids(1) == string"sireum")
        st"${(ops.ISZOps(ids).drop(2).map(encodeName), "_")}"
      else st"${(ids.map(encodeName), "_")}"
    return r
  }

  @pure def encodeName(id: String): String = {
    return id // TODO
  }

  @pure def typeName(tOpt: Option[AST.Typed]): QName = {
    return tOpt.get.asInstanceOf[AST.Typed.Name].ids
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

  @pure def filename(uriOpt: Option[String]): String = {
    uriOpt match {
      case Some(uri) =>
        val sops = ops.StringOps(uri)
        val i = sops.lastIndexOf('/')
        if (i < 0) {
          return uri
        }
        return sops.substring(i + 1, uri.size)
      case _ => return "main"
    }
  }

  @pure def filenameOfPosOpt(posOpt: Option[Position]): String = {
    posOpt match {
      case Some(pos) => return filename(pos.uriOpt)
      case _ => return filename(None())
    }
  }
}
