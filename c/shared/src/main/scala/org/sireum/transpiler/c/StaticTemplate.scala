// #Sireum
package org.sireum.transpiler.c

import org.sireum._
import org.sireum.lang.symbol.Resolver._

object StaticTemplate {

  @pure def genTypeH(stringMax: Z, typeNames: ISZ[ST]): ST = {
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

  @pure def genH(includes: ISZ[ST]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_H
      |#define SIREUM_GEN_H
      |
      |#include <misc.h>
      |
      |${(includes, "\n")}
      |
      |#endif"""
    return r
  }

  @pure def genC(typeNames: ISZ[ST]): ST = {
    val r =
      st"""#include <gen.h>
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
}
