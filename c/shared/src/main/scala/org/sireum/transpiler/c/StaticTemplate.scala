// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.lang.symbol.Resolver._

object StaticTemplate {

  @datatype class Compiled(typeHeader: ISZ[ST], header: ISZ[ST], impl: ISZ[ST])

  @pure def typeCompositeH(stringMax: Z, typeNames: ISZ[String]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_TYPE_H
      |#define SIREUM_GEN_TYPE_H
      |
      |#include <memory.h>
      |#include <ztype.h>
      |#include <stackframe.h>
      |
      |typedef enum {
      |  ${(for (tn <- typeNames) yield st"T$tn", ",\n")}
      |} TYPE;
      |
      |typedef struct Type *Type;
      |struct Type {
      |  TYPE type;
      |};
      |
      |#define STRING_MAX $stringMax
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
      |  C data[STRING_MAX + 1];
      |};
      |
      |#define string(v) &((struct String) { .type = TString, .size = Z_C(sizeof(v) - 1), .value = (C *) (v) })
      |
      |#define DeclString(x, e) struct StaticString x = e
      |#define DeclNewString(x) DeclString(x, ((struct StaticString) { .type = TYPE_String, .size = Z_C(0), .value = NULL, .data = {0} })); (x).value = (x).data
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

  @pure def typesC(typeNames: ISZ[String]): ST = {
    val r =
      st"""#include <types.h>
          |
      |size_t sizeOf(Type t) {
          |  switch (t->type) {
          |    ${(for (tn <- typeNames) yield st"case T$tn: return sizeof(struct $tn);", "\n")}
          |    default: exit(1);
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
        yield if (f.size == z"1") st"${f(0)}" else st"${ops.ISZOps(f).dropRight(1)}/${f(f.size - 1)}"
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

    @pure def main(filename: String): ST = {
      val r =
        st"""
        |add_executable($filename
        |        ${(files(filess), "\n")})
        |
        |target_include_directories($filename
        |        ${(includeDirs(filess), "\n")})"""
      return r
    }

    val mains: ISZ[ST] = for (f <- mainFilenames) yield main(f)

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

  @pure def compiled(compiledMap: HashMap[QName, Compiled]): ISZ[(QName, ST)] = {
    val entries = compiledMap.entries
    val sortedEntries = ops.ISZOps(entries).sortWith((p1, p2) => qnameLt(p1._1, p2._1))
    var r = ISZ[(QName, ST)]()
    for (e <- sortedEntries) {
      val dir = e._1
      val filename = st"${(e._1, "_")}"
      val comp = e._2
      val typeHeaderFilename = st"type-$filename.h".render
      val headerFilename = st"$filename.h".render
      val implFilename = st"$filename.c".render
      r = r :+ ((dir :+ typeHeaderFilename, st"""#ifndef SIREUM_TYPE_H_$filename
      |#define SIREUM_TYPE_H_$filename
      |#include <gentype.h>
      |
      |${(comp.typeHeader, "\n\n")}
      |
      |#endif"""))
      r = r :+ ((dir :+ headerFilename, st"""#ifndef SIREUM_H_$filename
      |#define SIREUM_H_$filename
      |#include <gen.h>
      |
      |${(comp.header, "\n\n")}
      |
      |#endif"""))
      r = r :+ ((dir :+ implFilename, st"""#include <all.h>
      |
      |${(comp.impl, "\n\n")}
      |
      |#endif"""))
    }
    return r
  }
}
