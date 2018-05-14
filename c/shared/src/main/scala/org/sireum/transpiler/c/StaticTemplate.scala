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
      |  C *value;
      |};
      |
      |struct StaticString {
      |  TYPE type;
      |  Z size;
      |  C *value;
      |  C data[MaxString + 1];
      |};
      |
      |#define string(v) &((struct String) { .type = TString, .size = Z_C(sizeof(v) - 1), .value = (C *) (v) })
      |
      |#define DeclString(x, e) struct StaticString x = e
      |#define DeclNewString(x) DeclString(x, ((struct StaticString) { .type = TYPE_String, .size = Z_C(0), .value = NULL, .data = {0} })); (x).value = (x).data
      |
      |#define MaxAString $isStringMax
      |
      |typedef struct AString *AString;
      |struct AString {
      |  TYPE type;
      |  Z size;
      |  struct StaticString value[MaxAString];
      |};
      |
      |#define DeclAString(x, e)    struct AString x = e
      |#define NewAString()         ((struct AString) { TYPE_AString, Z_C(0) })
      |#define DeclNewAString(x)    DeclAString(x, NewAString())
      |#define AString_size(this)   (this)->size
      |#define AString_up(this, i)  (this)->value[i]
      |#define AString_at(this, i)  (String) &(AString_up(this, i))
      |
      |#endif"""
    return r
  }

  @pure def typesH(names: ISZ[QName]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_H
      |#define SIREUM_GEN_H
      |
      |#include <misc.h>
      |${(for (name <- ops.ISZOps(names).sortWith(qnameLt)) yield st"#include <type-${(name, "_")}.h>", "\n")}
      |
      |#endif"""
    return r
  }

  @pure def typesC(typeNames: ISZ[(String, ST)]): ST = {
    val r =
      st"""#include <types.h>
      |
      |size_t sizeOf(Type t) {
      |  switch (t->type) {
      |    ${(for (tn <- typeNames) yield st"case T${tn._2}: return sizeof(struct ${tn._2}); // ${tn._1}", "\n")}
      |  }
      |}
      |
      |void clone(Type src, Type dest) {
      |  size_t srcSize = sizeOf(src);
      |  size_t destSize = sizeOf(dest);
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
    name: ST,
    indexType: ST,
    elementType: ST,
    elementTypePtr: ST,
    maxElement: Z
  ): ST = {
    val r =
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
      |#define Decl$name(x, e) struct $name x = e
      |#define New$name() ((struct $name) { TYPE_$name, ${indexType}_C(0) })
      |#define DeclNew$name(x) Decl$name(x, New$name())
      |#define ${name}_size(this) ((this)->size)
      |#define ${name}_up(this, i) ((this)->value[i])
      |#define ${name}_at(this, i) (($elementTypePtr) &(AString_up(this, i)))"""
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
