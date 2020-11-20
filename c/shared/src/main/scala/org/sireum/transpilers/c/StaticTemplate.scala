// #Sireum
/*
 Copyright (c) 2020, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.sireum.transpilers.c

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

  object Compiled {
    def empty: Compiled = {
      return Compiled(ISZ(), ISZ(), ISZ(), ISZ())
    }
  }

  @datatype class Compiled(typeHeader: ISZ[ST], header: ISZ[ST], impl: ISZ[ST], excludedImpl: ISZ[ST])

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

  @pure def typeCompositeH(stringMax: Z, isStringMax: Z, typeNames: ISZ[(String, ST, ST, ST)]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_TYPE_H
          |#define SIREUM_GEN_TYPE_H
          |
          |#ifdef __cplusplus
          |extern "C" {
          |#endif
          |#include <stackframe.h>
          |
          |typedef enum {
          |  ${
        (
          for (tn <- typeNames)
            yield st"T${tn._2} = ${tn._4}, // ${tn._1}",
          "\n"
        )
      }
          |} TYPE;
          |
          |char *TYPE_string_(void *type);
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
          |#define string(v) ((String) &((struct { TYPE type; Z size; C value[sizeof(v)]; }) { TString, Z_C(sizeof (v) - 1), v }))
          |#define DeclNewString(x) struct StaticString x = { .type = TString }
          |
          |#ifdef __cplusplus
          |}
          |#endif
          |
          |#endif"""
    return r
  }

  @pure def typesH(names: ISZ[QName], typeNames: ISZ[(String, ST, ST, ST)]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_H
          |#define SIREUM_GEN_H
          |
          |#ifdef __cplusplus
          |extern "C" {
          |#endif
          |
          |#include <misc.h>
          |${(for (name <- ops.ISZOps(names).sortWith(qnameLt _)) yield st"#include <type-${(name, "_")}.h>", "\n")}
          |
          |#if defined(static_assert)
          |#define STATIC_ASSERT static_assert
          |#define GLOBAL_STATIC_ASSERT(a, b, c) static_assert(b, c)
          |#else
          |#define STATIC_ASSERT(pred, explanation); {char assert[1/(pred)];(void)assert;}
          |#define GLOBAL_STATIC_ASSERT(unique, pred, explanation); namespace ASSERTION {char unique[1/(pred)];}
          |#endif
          |
          |size_t sizeOf(Type t);
          |void Type_assign(void *dest, void *src, size_t destSize);
          |
          |#ifdef __cplusplus
          |}
          |#endif
          |
          |#endif"""
    return r
  }

  @pure def typesC(typeNames: ISZ[(String, ST, ST, ST)]): ST = {
    val strings: ISZ[ST] = for (tn <- typeNames) yield st""""${tn._3}""""
    val r =
      st"""#include <types.h>
          |
          |size_t sizeOf(Type t) {
          |  TYPE type = t->type;
          |  switch (type) {
          |    ${(for (tn <- typeNames) yield st"case T${tn._2}: return sizeof(struct ${tn._2}); // ${tn._1}", "\n")}
          |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
          |  }
          |}
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
          |char *TYPE_string_(void *type) {
          |  static char *strings[] = {
          |    ${(strings, ",\n")}
          |  };
          |  return strings[((Type) type)->type];
          |}"""
    return r
  }

  @pure def allH(names: ISZ[QName], entries: ISZ[ST]): ST = {
    val r =
      st"""#ifndef SIREUM_ALL_H
          |#define SIREUM_ALL_H
          |
          |#ifdef __cplusplus
          |extern "C" {
          |#endif
          |
          |#include <types.h>
          |${(for (name <- ops.ISZOps(names).sortWith(qnameLt _)) yield st"#include <${(name, "_")}.h>", "\n")}
          |
          |${(entries, "\n")}
          |
          |#ifdef __cplusplus
          |}
          |#endif
          |
          |#endif"""
    return r
  }

  @pure def allC(typeNames: ISZ[(String, ST, ST, ST)], entries: ISZ[ST]): ST = {
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
          |  #ifndef SIREUM_NO_PRINT
          |  TYPE type = ((Type) this)->type;
          |  switch (type) {
          |    ${(for (tn <- typeNames) yield st"case T${tn._2}: ${tn._2}_cprint((${tn._2}) this, isOut); return;", "\n")}
          |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
          |  }
          |  #endif
          |}
          |
          |void Type_string_(STACK_FRAME String result, void *this) {
          |  TYPE type = ((Type) this)->type;
          |  switch (type) {
          |    ${
        (
          for (tn <- typeNames) yield st"case T${tn._2}: ${tn._2}_string_(CALLER result, (${tn._2}) this); return;",
          "\n"
        )
      }
          |    default: fprintf(stderr, "%s: %d\n", "Unexpected TYPE: ", type); exit(1);
          |  }
          |}
          |
          |${(entries, "\n\n")}"""
    return r
  }

  @pure def cmake(libOnly: B, project: String, stackSize: String, mainFilenames: ISZ[ISZ[String]], filess: ISZ[QName],
                  includes: ISZ[String], plusIncludes: ISZ[String]): ST = {
    val mainFs = HashSet ++ mainFilenames

    @pure def cSource(fs: QName): B = {
      val sops = ops.StringOps(fs(fs.size - 1))
      return (sops.endsWith(".c") || sops.endsWith(".h")) && !sops.endsWith("-excluded.c")
    }

    @pure def files(fss: ISZ[QName]): ISZ[ST] = {
      return for (f <- fss if !mainFs.contains(f) && cSource(f)) yield st"${(f, "/")}"
    }

    @pure def includeDirs(fss: ISZ[QName]): ISZ[ST] = {
      var r = HashSSet.empty[QName]
      for (f <- fss) {
        if (f.size == z"1") {
          r = r + ISZ(".")
        } else {
          r = r + ops.ISZOps(f).dropRight(1)
        }
      }
      return for (f <- r.elements) yield st"PUBLIC ${(f, "/")}"
    }

    @pure def target(filepath: ISZ[String]): ST = {
      val name = removeExt(filepath(filepath.size - 1))
      val r =
        st"""
            |add_executable($name-bin ${(filepath, "/")})
            |
            |target_link_libraries($name-bin LINK_PUBLIC $project m)
            |
            |set_target_properties($name-bin PROPERTIES OUTPUT_NAME $name)"""
      return r
    }

    val mains: ISZ[ST] = if (libOnly) ISZ[ST]() else for (f <- mainFilenames) yield target(f)

    val r =
      st"""cmake_minimum_required(VERSION 3.5.1)
          |
          |project($project)
          |
          |set(CMAKE_C_STANDARD 99)
          |
          |add_compile_options(-Werror)
          |
          |function(to_hex DEC HEX)
          |  while(DEC GREATER 0)
          |    math(EXPR _val "$${DEC} % 16")
          |    math(EXPR DEC "$${DEC} / 16")
          |    if(_val EQUAL 10)
          |      set(_val "A")
          |    elseif(_val EQUAL 11)
          |      set(_val "B")
          |    elseif(_val EQUAL 12)
          |      set(_val "C")
          |    elseif(_val EQUAL 13)
          |      set(_val "D")
          |    elseif(_val EQUAL 14)
          |      set(_val "E")
          |    elseif(_val EQUAL 15)
          |      set(_val "F")
          |    endif()
          |    set(_res "$${_val}$${_res}")
          |  endwhile()
          |  set($${HEX} "0x$${_res}" PARENT_SCOPE)
          |endfunction()
          |
          |if ("$${CMAKE_CXX_COMPILER_ID}" MATCHES "(C|c?)lang")
          |  to_hex("$stackSize" stack_size)
          |  set(CMAKE_EXE_LINKER_FLAGS "-Wl,-stack_size -Wl,$${stack_size}")
          |  add_compile_options("$$<$$<CONFIG:Release>:-Oz>")
          |elseif ("$${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
          |  if (WIN32 OR MINGW OR MSYS OR CYGWIN)
          |    to_hex("$stackSize" stack_size)
          |    set(CMAKE_EXE_LINKER_FLAGS "-Wl,--stack,$${stack_size}")
          |  endif()
          |  add_compile_options(-fstack-usage)
          |  add_compile_options("$$<$$<CONFIG:Release>:-Os>")
          |endif()
          |
          |option(BOUND_CHECK
          |  "Build the program with sequence bound checking."
          |  OFF)
          |
          |if(BOUND_CHECK)
          |  add_definitions(-DSIREUM_BOUND_CHECK)
          |endif(BOUND_CHECK)
          |
          |option(RANGE_CHECK
          |  "Build the program with range checking."
          |  OFF)
          |
          |if(RANGE_CHECK)
          |  add_definitions(-DSIREUM_RANGE_CHECK)
          |endif(RANGE_CHECK)
          |
          |option(NO_PRINT
          |  "Build the program without console output."
          |  OFF)
          |
          |if(NO_PRINT)
          |  add_definitions(-DSIREUM_NO_PRINT)
          |endif(NO_PRINT)
          |
          |option(WITH_LOC
          |  "Build the program with Slang location info."
          |  OFF)
          |
          |if(WITH_LOC)
          |  add_definitions(-DSIREUM_LOC)
          |endif(WITH_LOC)
          |
          |${(includes, "\n\n")}
          |
          |add_library($project STATIC
          |        ${(files(filess), "\n")})
          |
          |target_include_directories($project
          |        ${(for (d <- includeDirs(filess)) yield st"PUBLIC $d", "\n")})
          |
          |${(mains, "\n\n")}
          |
          |${(plusIncludes, "\n\n")}"""
    return r
  }

  @pure def typeManglingMap(entries: ISZ[(String, String)]): ST = {
    val sortedEntries = ops.ISZOps(entries).sortWith((p1, p2) => p1._1 < p2._1)
    return st"${(for (p <- sortedEntries) yield st"${p._1}=${p._2}", "\n")}"
  }

  @pure def sizesConfig(sizesMap: HashMap[AST.Typed.Name, Z]): ST = {
    @strictpure def toST(t: AST.Typed.Name): String =
      if (t.ids == AST.Typed.isName)
        if (t.args(0) == AST.Typed.z) s"ISZ[${t.args(1)}]" else s"IS[${t.args(0)}, ${t.args(1)}]"
      else
        if (t.args(0) == AST.Typed.z) s"MSZ[${t.args(1)}]" else s"MS[${t.args(0)}, ${t.args(1)}]"
    val sts: ISZ[ST] = for (e <- ops.ISZOps(sizesMap.entries.map((p: (AST.Typed.Name, Z)) => (toST(p._1), p._2))).
      sortWith((p1: (String, Z), p2: (String, Z)) => p1._1 <= p2._1)) yield st"${e._1}=${e._2}"
    return st"${(sts, ";\n")}"
  }

  @pure def qnameLt(qn1: QName, qn2: QName): B = {
    return st"$qn1".render < st"$qn2".render
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
      val excludedImplFilename = st"$filename-excluded.c".render
      r = r :+ ((dir :+ tHeaderFilename,
        st"""#ifndef SIREUM_TYPE_H_$filename
            |#define SIREUM_TYPE_H_$filename
            |
            |#ifdef __cplusplus
            |extern "C" {
            |#endif
            |
            |#include <misc.h>
            |
            |${(comp.typeHeader, "\n\n")}
            |
            |#ifdef __cplusplus
            |}
            |#endif
            |
            |#endif"""))
      r = r :+ ((dir :+ headerFilename,
        st"""#ifndef SIREUM_H_$filename
            |#define SIREUM_H_$filename
            |
            |#ifdef __cplusplus
            |extern "C" {
            |#endif
            |
            |#include <types.h>
            |
            |${(comp.header, "\n\n")}
            |
            |#ifdef __cplusplus
            |}
            |#endif
            |
            |#endif"""))
      if (comp.impl.nonEmpty) {
        r = r :+ ((dir :+ implFilename,
          st"""#include <all.h>
              |
              |${(comp.impl, "\n\n")}"""))
      }
      if (comp.excludedImpl.nonEmpty) {
        r = r :+ ((dir :+ excludedImplFilename,
          st"""#include <all.h>
              |
              |${(comp.excludedImpl, "\n\n")}"""))
      }
    }
    return r
  }

  @pure def worksheet(filename: String, preambles: ISZ[ST], stmts: ISZ[ST]): ST = {
    val r =
      st"""#include <all.h>
          |
          |${(preambles, "\n\n")}
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
    val r: ST = /* if (atExit.nonEmpty) {
      st"""#include <all.h>
          |#include <signal.h>
          |
          |void atExit(int signum) {
          |  ${(atExit, "\n")}
          |}
          |
          |int main(int argc, char *argv[]) {
          |  struct sigaction action;
          |  action.sa_handler = atExit;
          |  sigemptyset(&action.sa_mask);
          |  action.sa_flags = 0;
          |  sigaction(SIGTERM, &action, NULL);
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
          |    ${iszStringType}_at(&t_args, i)->type = TString;
          |    ${iszStringType}_at(&t_args, i)->size = (Z) argSize;
          |    memcpy(${iszStringType}_at(&t_args, i)->value, arg, argSize + 1);
          |  }
          |
          |  t_args.size = ($iszSizeType) size;
          |
          |  return (int) ${AST.Util.mangleName(owner)}_$id(SF &t_args);
          |}"""
    } else */ {
      st"""#include <all.h>
          |
          |int main(int argc, char *argv[]) {
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
          |    ${iszStringType}_at(&t_args, i)->type = TString;
          |    ${iszStringType}_at(&t_args, i)->size = (Z) argSize;
          |    memcpy(${iszStringType}_at(&t_args, i)->value, arg, argSize + 1);
          |  }
          |
          |  t_args.size = ($iszSizeType) size;
          |
          |  return (int) ${AST.Util.mangleName(owner)}_$id(SF &t_args);
          |}"""
    }
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
    val constructorHeader = st"void ${name}_apply(STACK_FRAME $name this$constructorParams)"
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
                   varTypes: ISZ[Vard],
                   defaultString: B
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
          |#define DeclNew$name(x) struct $name x = { .type = T$name }"""

    var accessors = ISZ[ST]()
    for (vd <- vars) {
      if (isScalar(vd.kind)) {
        accessors = accessors :+ st"#define ${name}_${vd.id}_(this) ((this)->${vd.id})"
        if (vd.isVar) {
          accessors = accessors :+ st"#define ${name}_${vd.id}_a(this, p_value) (this)->${vd.id} = (p_value)"
        }
      } else {
        accessors = accessors :+ st"#define ${name}_${vd.id}_(this) ((${vd.tpePtr}) &(this)->${vd.id})"
        if (vd.isVar) {
          accessors = accessors :+ st"#define ${name}_${vd.id}_a(this, p_value) Type_assign(&(this)->${vd.id}, p_value, sizeof(${vd.tpe}))"
        }
      }
    }

    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val neHeader =
      st"""inline B ${name}__ne($name this, $name other) {
          |  return !${name}__eq(this, other);
          |}"""
    val neImpl = st"B ${name}__ne($name this, $name other);"
    val stringHeader = st"void ${name}_string_(STACK_FRAME String result, $name this)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"

    val stringHeaderOpt: Option[ST] = if (defaultString) Some(st"$stringHeader;") else None()
    val header =
      st"""// $tpe
          |
          |${(accessors, "\n")}
          |
          |$eqHeader;
          |$neHeader;
          |$stringHeaderOpt
          |$cprintHeader;
          |
          |inline B ${name}__is(STACK_FRAME void* this) {
          |  return (($name) this)->type == T$name;
          |}
          |
          |inline $name ${name}__as(STACK_FRAME void *this) {
          |  if (${name}__is(CALLER this)) return ($name) this;
          |  sfAbortImpl(CALLER "Invalid cast to $tpe.");
          |  $abort
          |}"""

    var eqStmts = ISZ[ST]()
    var stringStmts = ISZ[ST]()
    var cprintStmts = ISZ[ST]()
    if (defaultString) {
      cprintStmts = cprintStmts :+ st"""String_cprint(string("${className(className.size - 1)}("), isOut);"""
      stringStmts = stringStmts :+ st"""DeclNewStackFrame(caller, "$uri", "${dotName(className)}", "string", 0);
                                       |String_string_(SF result, string("${className(className.size - 1)}("));"""
    } else {
      cprintStmts = cprintStmts :+ st"DeclNewString(temp);"
      cprintStmts = cprintStmts :+ st"""${name}_string_(SF (String) &temp, this);"""
      cprintStmts = cprintStmts :+ st"""String_cprint((String) &temp, this);"""
    }

    if (defaultString && constructorParamTypes.size > 1) {
      stringStmts = stringStmts :+ st"""String sep = string(", ");"""
      cprintStmts = cprintStmts :+ st"""String sep = string(", ");"""
    }

    for (i <- constructorParamTypes.indices) {
      val vd = constructorParamTypes(i)
      val pre: ST = if (isScalar(vd.kind)) st"" else st"(${vd.tpePtr}) &"
      if (!vd.isHidden) {
        eqStmts = eqStmts :+ st"if (${vd.tpePtr}__ne(${pre}this->${vd.id}, ${pre}other->${vd.id})) return F;"
      }
      if (defaultString) {
        if (i > 0) {
          stringStmts = stringStmts :+ st"String_string_(SF result, sep);"
          cprintStmts = cprintStmts :+ st"String_cprint(sep, isOut);"
        }
        stringStmts = stringStmts :+ st"${vd.tpePtr}_string_(SF result, ${pre}this->${vd.id});"
        cprintStmts = cprintStmts :+ st"${vd.tpePtr}_cprint(${pre}this->${vd.id}, isOut);"
      }
    }
    if (defaultString) {
      stringStmts = stringStmts :+ st"""String_string_(SF result, string(")"));"""
      cprintStmts = cprintStmts :+ st"""String_cprint(string(")"), isOut);"""
    }

    val stringImplOpt: Option[ST] =
      if (defaultString)
        Some(st"""
                 |$stringHeader {
                 |  ${(stringStmts, "\n")}
                 |}""")
      else None()
    val impl =
      st"""// $tpe
          |
          |$eqHeader {
          |  ${(eqStmts, "\n")}
          |  return T;
          |}
          |
          |$neImpl
          |$stringImplOpt
          |
          |$cprintHeader {
          |  #ifndef SIREUM_NO_PRINT
          |  ${(cprintStmts, "\n")}
          |  #endif
          |}
          |
          |B ${name}__is(STACK_FRAME void* this);
          |$name ${name}__as(STACK_FRAME void *this);"""
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
          |B ${name}__is(STACK_FRAME void *this);
          |$name ${name}__as(STACK_FRAME void *this);
          |inline void ${name}_string_(STACK_FRAME String result, $name this) {
          |  Type_string_(CALLER result, this);
          |}"""
    val impl =
      st"""// $tpe
          |
          |B ${name}__is(STACK_FRAME void *this) {
          |  switch(((Type) this)->type) {
          |    ${(for (t <- leafTypes) yield st"case T$t: return T;", "\n")}
          |    default: return F;
          |  }
          |}
          |
          |$name ${name}__as(STACK_FRAME void *this) {
          |  switch(((Type) this)->type) {
          |    ${(for (t <- leafTypes) yield st"case T$t: break;", "\n")}
          |    default:
          |      fprintf(stderr, "Invalid cast from %s to $tpe.", TYPE_string_(this));
          |      sfAbortImpl(CALLER "");
          |  }
          |  return ($name) this;
          |}
          |
          |void ${name}_string_(STACK_FRAME String result, $name this);"""
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
                   isBit: B,
                   isElementTypeScalar: B,
                   elementType: ST,
                   elementTypePtr: ST,
                   maxElement: Z
                 ): Compiled = {
    val offset: ST = if (minIndex == z"0") st"" else st"- ${indexType}_C($minIndex)"
    val sName: String = if (isImmutable) "IS" else "MS"
    val sizeType = arraySizeType(maxElement)
    val atH = st"$elementTypePtr ${name}_at($name this, $indexType i)"
    val upH = st"void ${name}_up($name this, $indexType i, $elementTypePtr e)"
    val (atHeader, upHeader): (ST, ST) =
      if (isBit)
        (
          st"""inline $atH {
              |  intmax_t idx = i$offset;
              |  #ifdef SIREUM_BOUND_CHECK
              |  assert (0 <= idx && idx < this->size);
              |  #endif
              |  U8 mask = (U8) (1 << (idx % 8));
              |  return ($elementType) (this->value[idx / 8] & mask ? 1 : 0);
              |}""",
          st"""inline $upH {
              |  intmax_t idx = i$offset;
              |  #ifdef SIREUM_BOUND_CHECK
              |  assert (0 <= idx && idx < this->size);
              |  #endif
              |  U8 mask = ((U8) (1 << idx % 8));
              |  if (e) {
              |    this->value[idx / 8] |= mask;
              |  } else {
              |    this->value[idx / 8] &= ~mask;
              |  }
              |}""")
      else if (isElementTypeScalar)
        (
          st"""inline $atH {
              |  intmax_t idx = i$offset;
              |  #ifdef SIREUM_BOUND_CHECK
              |  assert (0 <= idx && idx < this->size);
              |  #endif
              |  return this->value[($sizeType) idx];
              |}""",
          st"""inline $upH {
              |  intmax_t idx = i$offset;
              |  #ifdef SIREUM_BOUND_CHECK
              |  assert (0 <= idx && idx < this->size);
              |  #endif
              |  this->value[($sizeType) idx] = e;
              |}""")
      else
        (
          st"""inline $atH {
              |  intmax_t idx = i$offset;
              |  #ifdef SIREUM_BOUND_CHECK
              |  assert (0 <= idx && idx < this->size);
              |  #endif
              |  return ($elementTypePtr) &(this->value[($sizeType) idx]);
              |}""",
          st"""inline $upH {
              |  intmax_t idx = i$offset;
              |  #ifdef SIREUM_BOUND_CHECK
              |  assert (0 <= idx && idx < this->size);
              |  #endif
              |  Type_assign(&this->value[($sizeType) idx], e, sizeof($elementType));
              |}""")
    val atImpl: ST = st"$atH;"
    val upImpl: ST = st"$upH;"
    val (toOtherHeaderOpt, toOtherImplOpt): (Option[ST], Option[ST]) = otherNameOpt match {
      case Some(otherName) =>
        val other: String = if (isImmutable) "MS" else "IS"
        (
          Some(
            st"""
                |inline void ${name}_to$other(STACK_FRAME $otherName result, $name this) {
                |  STATIC_ASSERT(Max$otherName >= Max$name, "Invalid cast from $tpe to $other[...,...].");
                |  result->type = T$otherName;
                |  result->size = this->size;
                |  memcpy(&result->value, &this->value, this->size ${if (isBit) st"/ 8 + 1" else st"* sizeof($elementType)"});
                |}"""
          ),
          Some(
            st"""
                |void ${name}_to$other(STACK_FRAME $otherName result, $name this);"""
          )
        )
      case _ => (None(), None())
    }
    val typeHeader =
      st"""// $tpe
          |${(includes, "\n")}
          |
          |#define Max$name $maxElement
          |#define ${name}SizeT ${if (isBit) "intmax_t" else sizeType}
          |
          |typedef struct $name *$name;
          |struct $name {
          |  TYPE type;
          |  ${name}SizeT size;
          |  ${if (isBit) "U8" else elementType} value[Max$name${if (isBit) " / 8 + 1" else ""}];
          |};
          |
          |#define DeclNew$name(x) struct $name x = { .type = T$name }"""
    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val createHeader = st"void ${name}_create(STACK_FRAME $name result, $indexType size, $elementTypePtr dflt)"
    val zreateHeader = st"void ${name}_zreate(STACK_FRAME $name result, Z size, $elementTypePtr dflt)"
    val appendHeader = st"void ${name}__append(STACK_FRAME $name result, $name this, $elementTypePtr value)"
    val prependHeader = st"void ${name}__prepend(STACK_FRAME $name result, $name this, $elementTypePtr value)"
    val appendAllHeader = st"void ${name}__appendAll(STACK_FRAME $name result, $name this, $name other)"
    val removeHeader = st"void ${name}__sub(STACK_FRAME $name result, $name this, $elementTypePtr value)"
    val removeAllHeader = st"void ${name}__removeAll(STACK_FRAME $name result, $name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string_(STACK_FRAME String result, $name this)"
    val header =
      st"""// $tpe
          |
          |$atHeader
          |
          |$upHeader
          |
          |inline $indexType ${name}_size(STACK_FRAME $name this) {
          |   return ($indexType) (this)->size;
          |}
          |
          |inline Z ${name}_zize(STACK_FRAME $name this) {
          |   return (Z) (this)->size;
          |}
          |
          |inline B ${name}_isEmpty(STACK_FRAME $name this) {
          |   return (this)->size == 0;
          |}
          |
          |inline B ${name}_nonEmpty(STACK_FRAME $name this) {
          |   return (this)->size != 0;
          |}
          |
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
          |inline B ${name}__ne($name this, $name other) {
          |  return !${name}__eq(this, other);
          |}
          |$toOtherHeaderOpt"""
    val impl: ST =
      if (isBit)
        st"""// $tpe
            |$atImpl
            |$upImpl
            |$indexType ${name}_size(STACK_FRAME $name this);
            |Z ${name}_zize(STACK_FRAME $name this);
            |B ${name}_isEmpty(STACK_FRAME $name this);
            |B ${name}_nonEmpty(STACK_FRAME $name this);
            |
            |$eqHeader {
            |  intmax_t size = this->size;
            |  if (size != other->size) return F;
            |  intmax_t n = size / 8;
            |  for (intmax_t i = 0; i < n; i++) {
            |    if (this->value[i] != other->value[i]) return F;
            |  }
            |  intmax_t m = size % 8;
            |  U8 mask = 1;
            |  for (intmax_t i = 0; i < m; i++) {
            |    if ((this->value[n] & mask) != (other->value[n] & mask)) return F;
            |    mask <<= 1;
            |  }
            |  return T;
            |}
            |
            |$createHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "create", 0);
            |  sfAssert((Z) size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  intmax_t zize = (intmax_t) size;
            |  U8 d = (U8) (dflt ? -1 : 0);
            |  intmax_t n = zize / 8;
            |  for (intmax_t i = 0; i < n; i++) {
            |    result->value[i] = d;
            |  }
            |  intmax_t m = size % 8;
            |  U8 mask = 1;
            |  if (dflt) {
            |    for (intmax_t i = 0; i < m; i++) {
            |      result->value[n] |= mask;
            |      mask <<= 1;
            |    }
            |  } else {
            |    for (intmax_t i = 0; i < m; i++) {
            |      result->value[n] &= ~mask;
            |      mask <<= 1;
            |    }
            |  }
            |  result->size = zize;
            |}
            |
            |$zreateHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "zreate", 0);
            |  sfAssert((Z) size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  intmax_t zize = (intmax_t) size;
            |  U8 d = (U8) (dflt ? -1 : 0);
            |  intmax_t n = zize / 8;
            |  for (intmax_t i = 0; i < n; i++) {
            |    result->value[i] = d;
            |  }
            |  intmax_t m = size % 8;
            |  U8 mask = 1;
            |  if (dflt) {
            |    for (intmax_t i = 0; i < m; i++) {
            |      result->value[n] |= mask;
            |      mask <<= 1;
            |    }
            |  } else {
            |    for (intmax_t i = 0; i < m; i++) {
            |      result->value[n] &= ~mask;
            |      mask <<= 1;
            |    }
            |  }
            |  result->size = zize;
            |}
            |
            |$appendHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", ":+", 0);
            |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
            |  intmax_t thisSize = this->size;
            |  Type_assign(result, this, sizeof(struct $name));
            |  ${name}_up(result, thisSize, value);
            |  result->size = (intmax_t) (thisSize + 1);
            |}
            |
            |$prependHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
            |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
            |  intmax_t thisSize = this->size;
            |  ${name}_up(result, 0, value);
            |  for (intmax_t i = 0; i < thisSize; i++)
            |    ${name}_up(result, i + 1, ${name}_at(this, i));
            |  result->size = (intmax_t) (thisSize + 1);
            |}
            |
            |$appendAllHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
            |  sfAssert(this->size + other->size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  intmax_t thisSize = this->size;
            |  intmax_t otherSize = other->size;
            |  Type_assign(result, this, sizeof(struct $name));
            |  result->size = ($sizeType) (thisSize + otherSize);
            |  for ($sizeType i = 0; i < otherSize; i++)
            |    ${name}_up(result, thisSize + i, ${name}_at(other, i));
            |}
            |
            |$removeHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "-", 0);
            |  intmax_t thisSize = this->size;
            |  intmax_t k = 0;
            |  for (intmax_t i = 0; i < thisSize; i++) {
            |    $elementTypePtr o = ${name}_at(this, i);
            |    if (${elementTypePtr}__ne(o, value)) ${name}_up(result, k++, o);
            |  }
            |  result->size = k;
            |}
            |
            |$removeAllHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "--", 0);
            |  intmax_t thisSize = this->size;
            |  intmax_t otherSize = other->size;
            |  intmax_t k = 0;
            |  for (intmax_t i = 0; i < thisSize; i++) {
            |    B found = F;
            |    $elementTypePtr o = ${name}_at(this, i);
            |    for (intmax_t j = 0; j < otherSize && !found; j++)
            |      if (${elementTypePtr}__eq(o, ${name}_at(other, j))) found = T;
            |    if (!found) ${name}_up(result, k++, o);
            |  }
            |  result->size = k;
            |}
            |
            |$cprintHeader {
            |  #ifndef SIREUM_NO_PRINT
            |  String_cprint(string("["), isOut);
            |  intmax_t size = this->size;
            |  intmax_t n = size / 8;
            |  for (intmax_t i = 0; i < n; i++) {
            |    U8_cprint(this->value[i], isOut);
            |  }
            |  if (size % 8 != 0) {
            |    U8_cprint(this->value[n], isOut);
            |  }
            |  String_cprint(string("]"), isOut);
            |  #endif
            |}
            |
            |$stringHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
            |  String_string_(SF result, string("["));
            |  intmax_t size = this->size;
            |  intmax_t n = size / 8;
            |  for (intmax_t i = 0; i < n; i++) {
            |    U8_string_(SF result, this->value[i]);
            |  }
            |  if (size % 8 != 0) {
            |    U8_string_(SF result, this->value[n]);
            |  }
            |  String_string_(SF result, string("]"));
            |}
            |
            |B ${name}__ne($name this, $name other);
            |$toOtherImplOpt"""
      else if (isElementTypeScalar)
        st"""// $tpe
            |$atImpl
            |$upImpl
            |$indexType ${name}_size(STACK_FRAME $name this);
            |Z ${name}_zize(STACK_FRAME $name this);
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
            |  sfAssert((Z) size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  $sizeType zize = ($sizeType) size;
            |  for ($sizeType i = 0; i < zize; i++) {
            |    result->value[i] = dflt;
            |  }
            |  result->size = zize;
            |}
            |
            |$zreateHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "zreate", 0);
            |  sfAssert((Z) size <= Max$name, "Insufficient maximum for $tpe elements.");
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
            |  result->size = ($sizeType) (thisSize + 1);
            |}
            |
            |$prependHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
            |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
            |  $sizeType thisSize = this->size;
            |  result->value[0] = value;
            |  for ($sizeType i = 0; i < thisSize; i++)
            |    result->value[i + 1] = this->value[i];
            |  result->size = ($sizeType) (thisSize + 1);
            |}
            |
            |$appendAllHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
            |  sfAssert(this->size + other->size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  $sizeType thisSize = this->size;
            |  $sizeType otherSize = other->size;
            |  Type_assign(result, this, sizeof(struct $name));
            |  result->size = ($sizeType) (thisSize + otherSize);
            |  for ($sizeType i = 0; i < otherSize; i++)
            |    result->value[thisSize + i] = other->value[i + 1];
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
            |  #ifndef SIREUM_NO_PRINT
            |  String_cprint(string("["), isOut);
            |  $sizeType size = this->size;
            |  if (size > 0) {
            |    $elementType *value = this->value;
            |    ${elementTypePtr}_cprint(value[0], isOut);
            |    for ($sizeType i = 1; i < size; i++) {
            |      String_cprint(string(", "), isOut);
            |      ${elementTypePtr}_cprint(value[i], isOut);
            |    }
            |  }
            |  String_cprint(string("]"), isOut);
            |  #endif
            |}
            |
            |$stringHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
            |  String_string_(SF result, string("["));
            |  $sizeType size = this->size;
            |  if (size > 0) {
            |    $elementType *value = this->value;
            |    ${elementTypePtr}_string_(SF result, value[0]);
            |    for ($sizeType i = 1; i < size; i++) {
            |      String_string_(SF result, string(", "));
            |      ${elementTypePtr}_string_(SF result, value[i]);
            |    }
            |  }
            |  String_string_(SF result, string("]"));
            |}
            |
            |B ${name}__ne($name this, $name other);
            |$toOtherImplOpt"""
      else
        st"""// $tpe
            |$atImpl
            |$upImpl
            |$indexType ${name}_size(STACK_FRAME $name this);
            |Z ${name}_zize(STACK_FRAME $name this);
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
            |  sfAssert((Z) size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  $sizeType zize = ($sizeType) size;
            |  for ($sizeType i = 0; i < zize; i++) {
            |    Type_assign(&result->value[i], dflt, sizeof($elementType));
            |  }
            |  result->size = zize;
            |}
            |
            |$zreateHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "zreate", 0);
            |  sfAssert((Z) size <= Max$name, "Insufficient maximum for $tpe elements.");
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
            |  Type_assign(result, this, sizeof(struct $name));
            |  Type_assign(&result->value[thisSize], value, sizeof($elementType));
            |  result->size = ($sizeType) (thisSize + 1);
            |}
            |
            |$prependHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "+:", 0);
            |  sfAssert(this->size + 1 <= Max$name, "Insufficient maximum for $tpe elements.");
            |  $sizeType thisSize = this->size;
            |  Type_assign(&result->value[0], value, sizeof($elementType));
            |  for ($sizeType i = 0; i < thisSize; i++)
            |    Type_assign(&result->value[i + 1], &this->value[i], sizeof($elementType));
            |  result->size = ($sizeType) thisSize + 1;
            |}
            |
            |$appendAllHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "++", 0);
            |  sfAssert(this->size + other->size <= Max$name, "Insufficient maximum for $tpe elements.");
            |  $sizeType thisSize = this->size;
            |  $sizeType otherSize = other->size;
            |  Type_assign(result, this, sizeof(struct $name));
            |  result->size = ($sizeType) thisSize + otherSize;
            |  for ($sizeType i = 0; i < otherSize; i++)
            |    Type_assign(&result->value[thisSize + i], &other->value[i + 1], sizeof($elementType));
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
            |  #ifndef SIREUM_NO_PRINT
            |  String_cprint(string("["), isOut);
            |  $sizeType size = this->size;
            |  if (size > 0) {
            |    $elementType *value = this->value;
            |    ${elementTypePtr}_cprint(&value[0], isOut);
            |    for ($sizeType i = 1; i < size; i++) {
            |      String_cprint(string(", "), isOut);
            |      ${elementTypePtr}_cprint(&value[i], isOut);
            |    }
            |  }
            |  String_cprint(string("]"), isOut);
            |  #endif
            |}
            |
            |$stringHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
            |  String_string_(SF result, string("["));
            |  $sizeType size = this->size;
            |  if (size > 0) {
            |    $elementType *value = this->value;
            |    ${elementTypePtr}_string_(SF result, ($elementTypePtr) &(value[0]));
            |    for ($sizeType i = 1; i < size; i++) {
            |      String_string_(SF result, string(", "));
            |      ${elementTypePtr}_string_(SF result, ($elementTypePtr) &(value[i]));
            |    }
            |  }
            |  String_string_(SF result, string("]"));
            |}
            |
            |B ${name}__ne($name this, $name other);
            |$toOtherImplOpt"""
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ header,
      impl = compiled.impl :+ impl
    )
  }

  @pure def elementName(owner: QName, id: String): ST = {
    return st"${AST.Util.mangleName(owner :+ "Type")}_${enumId(id)}"
  }

  @pure def enum(
                  compiled: Compiled,
                  uri: String,
                  name: QName,
                  elements: ISZ[String],
                  optElementTypeOpt: Option[(ST, ST, ST)],
                  iszElementTypeOpt: Option[ST]
                ): Compiled = {
    val mangledName = AST.Util.mangleName(name)

    @pure def enumCase(element: String): ST = {
      val r = st"""case ${mangledName}_${enumId(element)}: String_assign(result, string("$element")); return;"""
      return r
    }

    @pure def elemName(id: String): ST = {
      return st"${mangledName}_${enumId(id)}"
    }

    val indices = elements.indices.map((n: Z) => n)

    val tpe = dotName(name)

    val typeHeader =
      st"""// $tpe
          |typedef enum {
          |  ${
        (
          for (e <- ops.ISZOps(elements).zip(elements.indices.map((n: Z) => n)))
            yield st"${elemName(e._1)} = ${e._2}",
          ",\n"
        )
      }
          |} $mangledName;
          |
          |inline B ${mangledName}__eq($mangledName this, $mangledName other) {
          |  return this == other;
          |}
          |
          |inline B ${mangledName}__ne($mangledName this, $mangledName other) {
          |  return this != other;
          |}
          |
          |inline Z ${mangledName}__ordinal($mangledName this) {
          |  return (Z) this;
          |}
          |
          |inline void ${mangledName}_name_(String result, $mangledName this) {
          |  switch (this) {
          |    ${(for (e <- elements) yield enumCase(e), "\n")}
          |  }
          |}"""

    var header: ISZ[ST] = ISZ()
    var impl: ISZ[ST] = ISZ()

    optElementTypeOpt match {
      case Some((optElementType, someElementType, noneElementType)) =>
        val byNameHeader = st"void ${mangledName}_byName(STACK_FRAME $optElementType result, String s)"
        header = header :+ byNameHeader
        impl = impl :+
          st"""$byNameHeader {
              |  ${
            (
              for (e <- elements)
                yield
                  st"""if (String__eq(s, string("$e"))) Type_assign(result, &((struct $someElementType) { .type = T$someElementType, .value = ${
                    elemName(
                      e
                    )
                  } }), sizeof(union $optElementType));""",
              "\nelse "
            )
          }
              |  else Type_assign(result, &((struct $noneElementType) { .type = T$noneElementType }), sizeof(union $optElementType));
              |}"""

        val byOrdinalHeader = st"void ${mangledName}_byOrdinal(STACK_FRAME $optElementType result, Z n)"
        header = header :+ byOrdinalHeader
        impl = impl :+
          st"""$byOrdinalHeader {
              |  switch (($mangledName) n) {
              |    ${
            (
              for (e <- elements)
                yield
                  st"""case ${elemName(e)}: Type_assign(result, &((struct $someElementType) { .type = T$someElementType, .value = ${
                    elemName(
                      e
                    )
                  } }), sizeof(union $optElementType)); return;""",
              "\n"
            )
          }
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
              |  ${
            (
              for (p <- ops.ISZOps(elements).zip(indices)) yield st"result->value[${p._2}] = ${elemName(p._1)};",
              "\n"
            )
          }
              |}"""
      case _ =>
    }

    val numOfElementsHeader = st"Z ${mangledName}_numOfElements()"
    header = header :+ numOfElementsHeader
    impl = impl :+
      st"""$numOfElementsHeader {
          |  return Z_C(${elements.size});
          |}"""

    val cprintHeader = st"void ${mangledName}_cprint($mangledName this, B isOut)"
    header = header :+ cprintHeader
    impl = impl :+
      st"""$cprintHeader {
          |  #ifndef SIREUM_NO_PRINT
          |  switch (this) {
          |    ${
        (
          for (e <- elements) yield st"""case ${elemName(e)}: String_cprint(string("$e"), isOut); return;""",
          "\n"
        )
      }
          |  }
          |  #endif
          |}"""

    val stringHeader = st"void ${mangledName}_string_(STACK_FRAME String result, $mangledName this)"
    header = header :+ stringHeader
    impl = impl :+
      st"""$stringHeader {
          |  DeclNewStackFrame(caller, "$uri", "$mangledName", "string", 0);
          |  switch (this) {
          |    ${
        (
          for (e <- elements) yield st"""case ${elemName(e)}: String_string_(SF result, string("$e")); return;""",
          "\n"
        )
      }
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
            |B ${mangledName}__eq($mangledName this, $mangledName other);
            |B ${mangledName}__ne($mangledName this, $mangledName other);
            |Z ${mangledName}__ordinal($mangledName this);
            |void ${mangledName}_name_(String result, $mangledName this);
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
    val mangledName = AST.Util.mangleName(name)
    val cType = st"${if (isUnsigned) "u" else ""}int${bitWidth}_t"
    val cTypeUp = st"${if (isUnsigned) "U" else ""}INT$bitWidth"
    val pr = st"PRI${if (isUnsigned) if (isBitVector) "X" else "u" else "d"}$bitWidth"
    val min: ST = minOpt match {
      case Some(m) => st"${mangledName}_C($m)"
      case _ => if (isUnsigned) st"${mangledName}_C(0)" else st"${cTypeUp}_MIN"
    }
    val max: ST = maxOpt match {
      case Some(m) => st"${mangledName}_C($m)"
      case _ => st"${cTypeUp}_MAX"
    }
    val (shiftHeader, shiftImpl): (ST, ST) =
      if (!isBitVector) {
        (st"", st"")
      } else {
        val impl =
          st"""
              |$mangledName ${mangledName}__complement($mangledName n);
              |$mangledName ${mangledName}__shl($mangledName n1, $mangledName n2);
              |$mangledName ${mangledName}__shr($mangledName n1, $mangledName n2);
              |$mangledName ${mangledName}__ushr($mangledName n1, $mangledName n2);
              |$mangledName ${mangledName}__and($mangledName n1, $mangledName n2);
              |$mangledName ${mangledName}__or($mangledName n1, $mangledName n2);
              |$mangledName ${mangledName}__xor($mangledName n1, $mangledName n2);"""
        if (isUnsigned) {
          (
            st"""
                |inline $mangledName ${mangledName}__complement($mangledName n) {
                |  return ${mangledName}_range(($mangledName) ~n);
                |}
                |
                |inline $mangledName ${mangledName}__shl($mangledName n1, $mangledName n2) {
                |  return ${mangledName}_range(($mangledName) (n1 << n2));
                |}
                |
                |inline $mangledName ${mangledName}__shr($mangledName n1, $mangledName n2) {
                |  return ${mangledName}_range(($mangledName) (n1 >> n2));
                |}
                |
                |inline $mangledName ${mangledName}__ushr($mangledName n1, $mangledName n2) {
                |  return ${mangledName}_range(($mangledName) (n1 >> n2));
                |}
                |
                |inline $mangledName ${mangledName}__and($mangledName n1, $mangledName n2) {
                |  return ${mangledName}_range(($mangledName) (n1 & n2));
                |}
                |
                |inline $mangledName ${mangledName}__or($mangledName n1, $mangledName n2) {
                |  return ${mangledName}_range(($mangledName) (n1 | n2));
                |}
                |
                |inline $mangledName ${mangledName}__xor($mangledName n1, $mangledName n2) {
                |  return ${mangledName}_range(($mangledName) (n1 ^ n2));
                |}""", impl)
        } else {
          val unsigned: String = bitWidth match {
            case z"8" => "uint8_t"
            case z"16" => "uint16_t"
            case z"32" => "uint32_t"
            case z"64" => "uint64_t"
          }
          (
            st"""
                |inline $mangledName ${mangledName}__complement($mangledName n) {
                |  $unsigned un = ($unsigned) n;
                |  return ($mangledName) ~un;
                |}
                |
                |inline $mangledName ${mangledName}__shl($mangledName n1, $mangledName n2) {
                |  $unsigned un1 = ($unsigned) n1;
                |  $unsigned un2 = ($unsigned) n2;
                |  return ${mangledName}_range(($mangledName) (un1 << un2));
                |}
                |
                |inline $mangledName ${mangledName}__shr($mangledName n1, $mangledName n2) {
                |  $unsigned un1 = ($unsigned) n1;
                |  $unsigned un2 = ($unsigned) n2;
                |  return ${mangledName}_range(($mangledName) (un1 >> un2));
                |}
                |
                |inline $mangledName ${mangledName}__ushr($mangledName n1, $mangledName n2) {
                |  $unsigned un1 = ($unsigned) n1;
                |  $unsigned un2 = ($unsigned) n2;
                |  return ${mangledName}_range(($mangledName) (un1 >> un2));
                |}
                |
                |inline $mangledName ${mangledName}__and($mangledName n1, $mangledName n2) {
                |  $unsigned un1 = ($unsigned) n1;
                |  $unsigned un2 = ($unsigned) n2;
                |  return ${mangledName}_range(($mangledName) (un1 & un2));
                |}
                |
                |inline $mangledName ${mangledName}__or($mangledName n1, $mangledName n2) {
                |  $unsigned un1 = ($unsigned) n1;
                |  $unsigned un2 = ($unsigned) n2;
                |  return ${mangledName}_range(($mangledName) (un1 | un2));
                |}
                |
                |inline $mangledName ${mangledName}__xor($mangledName n1, $mangledName n2) {
                |  $unsigned un1 = ($unsigned) n1;
                |  $unsigned un2 = ($unsigned) n2;
                |  return ${mangledName}_range(($mangledName) (un1 ^ un2));
                |}""", impl)
        }
      }
    val hex: ST = if (isBitVector && isUnsigned) st"0${bitWidth / 4}" else st""
    val typeHeader =
      st"""typedef $cType $mangledName;
          |
          |#define ${mangledName}_C(n) ${cTypeUp}_C(n)
          |
          |#define ${mangledName}_Min ${if (isUnsigned) st"${cTypeUp}_C(0)" else st"${cTypeUp}_MIN"}
          |#define ${mangledName}_Max ${cTypeUp}_MAX
          |
          |#define ${mangledName}_F "%$hex" $pr """""
    val stringHeader = st"void ${mangledName}_string_(STACK_FRAME String result, $mangledName this)"
    var header = ISZ[ST]()
    var impl = ISZ[ST]()
    def addCheck(): Unit = {
      header = header :+
        st"""inline $mangledName ${mangledName}_range($mangledName n) {
            |  #ifdef SIREUM_RANGE_CHECK
            |  assert($min <= n && n <= $max);
            |  #endif
            |  return n;
            |}"""
      impl = impl :+ st"$mangledName ${mangledName}_range($mangledName n);"
    }
    if (isBitVector) {
      val (bitMin, bitMax): (Z, Z) = bitWidth match {
        case z"8" => if (isUnsigned) (0, 255) else (-128, 127)
        case z"16" => if (isUnsigned) (0, 65535) else (-32768, 32767)
        case z"32" => if (isUnsigned) (0, 4294967295L) else (-2147483648, 2147483647)
        case z"64" => if (isUnsigned) (0, z"18,446,744,073,709,551,615") else (-9223372036854775808L, 9223372036854775807L)
      }
      if (minOpt.getOrElse(bitMin) == bitMin && maxOpt.getOrElse(bitMax) == bitMax) {
        header = header :+ st"#define ${mangledName}_range(n) n"
      } else {
        addCheck()
      }
    } else {
      addCheck()
    }
    header = header :+
      st"""inline $mangledName ${mangledName}__plus($mangledName n) {
          |  return ${mangledName}_range(n);
          |}
          |
          |inline $mangledName ${mangledName}__minus($mangledName n) {
          |  return ${mangledName}_range(($mangledName) -n);
          |}
          |
          |inline $mangledName ${mangledName}__add($mangledName n1, $mangledName n2) {
          |  return ${mangledName}_range(($mangledName) (n1 + n2));
          |}
          |
          |inline $mangledName ${mangledName}__sub($mangledName n1, $mangledName n2) {
          |  return ${mangledName}_range(($mangledName) (n1 - n2));
          |}
          |
          |inline $mangledName ${mangledName}__mul($mangledName n1, $mangledName n2) {
          |  return ${mangledName}_range(($mangledName) (n1 * n2));
          |}
          |
          |inline $mangledName ${mangledName}__div($mangledName n1, $mangledName n2) {
          |  return ${mangledName}_range(($mangledName) (n1 / n2));
          |}
          |
          |inline $mangledName ${mangledName}__rem($mangledName n1, $mangledName n2) {
          |  return ${mangledName}_range(($mangledName) (n1 % n2));
          |}
          |
          |inline B ${mangledName}__eq($mangledName n1, $mangledName n2) {
          |  return (B) (n1 == n2);
          |}
          |
          |inline B ${mangledName}__ne($mangledName n1, $mangledName n2) {
          |  return (B) (n1 != n2);
          |}
          |
          |inline B ${mangledName}__lt($mangledName n1, $mangledName n2) {
          |  return (B) (n1 < n2);
          |}
          |
          |inline B ${mangledName}__le($mangledName n1, $mangledName n2) {
          |  return (B) (n1 <= n2);
          |}
          |
          |inline B ${mangledName}__gt($mangledName n1, $mangledName n2) {
          |  return (B) (n1 > n2);
          |}
          |
          |inline B ${mangledName}__ge($mangledName n1, $mangledName n2) {
          |  return (B) (n1 >= n2);
          |}
          |$shiftHeader
          |
          |#ifdef SIREUM_NO_PRINT
          |#define ${mangledName}_cprint(this, isOut)
          |#else
          |#define ${mangledName}_cprint(this, isOut) { if (isOut) printf(${mangledName}_F, this); else fprintf(stderr, ${mangledName}_F, this); }
          |#endif
          |$stringHeader"""
    impl = impl :+
      st"""$mangledName ${mangledName}__plus($mangledName n);
          |$mangledName ${mangledName}__minus($mangledName n);
          |$mangledName ${mangledName}__add($mangledName n1, $mangledName n2);
          |$mangledName ${mangledName}__sub($mangledName n1, $mangledName n2);
          |$mangledName ${mangledName}__mul($mangledName n1, $mangledName n2);
          |$mangledName ${mangledName}__div($mangledName n1, $mangledName n2);
          |$mangledName ${mangledName}__rem($mangledName n1, $mangledName n2);
          |B ${mangledName}__eq($mangledName n1, $mangledName n2);
          |B ${mangledName}__ne($mangledName n1, $mangledName n2);
          |B ${mangledName}__lt($mangledName n1, $mangledName n2);
          |B ${mangledName}__le($mangledName n1, $mangledName n2);
          |B ${mangledName}__gt($mangledName n1, $mangledName n2);
          |B ${mangledName}__ge($mangledName n1, $mangledName n2);
          |$shiftImpl
          |
          |$stringHeader {
          |  DeclNewStackFrame(caller, "$uri", "${dotName(name)}", "string", 0);
          |  int nSize = snprintf(NULL, 0, ${mangledName}_F, this);
          |  Z size = result->size;
          |  Z newSize = size + nSize;
          |  sfAssert(newSize <= MaxString, "Insufficient maximum for String characters.");
          |  snprintf(&(result->value[result->size]), nSize + 1, ${mangledName}_F, this);
          |  result->size = newSize;
          |}"""
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
      header = compiled.header :+
        st"""#include <lib.h>
            |
            |${(header, ";\n")};""",
      impl = compiled.impl :+
        st"""#include <lib.h>
            |
            |${(impl, "\n\n")}"""
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
    val mangledName = AST.Util.mangleName(name)
    val header = st"void ${mangledName}_apply($optName result, String s)"
    val base: ST = if (hasBase) st", 0" else st""
    val rangeCheck: ST = if (hasRange) st" && $min <= n && n <= $max" else st""
    val impl =
      st"""$header {
          |  char *endptr;
          |  errno = 0;
          |  $cType n = $cStrTo(s->value, &endptr$base);
          |  if (errno) {
          |    errno = 0;
          |    Type_assign(result, &((struct $noneName) { .type = T$noneName }), sizeof(struct $noneName));
          |    return;
          |  }
          |  if (&s->value[s->size] - endptr == 0$rangeCheck)
          |    Type_assign(result, &((struct $someName) { .type = T$someName, .value = ($mangledName) n }), sizeof(struct $someName));
          |  else Type_assign(result, &((struct $noneName) { .type = T$noneName }), sizeof(struct $noneName));
          |}"""
    return (st"$header;", impl)
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
          |  if (noResult) Type_assign(result, &((struct $noneName) { .type = T$noneName }), sizeof(struct $noneName));
          |  else if (r) Type_assign(result, &((struct $someName) { .type = T$someName, .value = T }), sizeof(struct $someName));
          |  else Type_assign(result, &((struct $someName) { .type = T$someName, .value = F }), sizeof(struct $someName));
          |}"""
    return (st"$header;", impl)
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
    val constructorHeader = st"void ${name}_apply(STACK_FRAME $name this, ${(cParams, ", ")})"
    val typesIndices: ISZ[((TypeKind.Type, ST, ST), Z)] = ops.ISZOps(constructorParamTypes).zip(indices)
    var accessorHeaders = ISZ[ST]()
    var accessorImpls = ISZ[ST]()
    var constructorStmts = ISZ[ST]()
    for (p <- typesIndices) {
      val ((kind, _, tPtr), i) = p
      val index = i + 1
      if (isScalar(kind)) {
        accessorHeaders = accessorHeaders :+
          st"""inline $tPtr ${name}_$index($name this) {
              |  return this->_$index;
              |}"""
        constructorStmts = constructorStmts :+ st"this->_$index = _$index;"
      } else {
        val us: String = if (isTrait(kind)) "union" else "struct"
        accessorHeaders = accessorHeaders :+
          st"""inline $tPtr ${name}_$index($name this) {
              |  return ($tPtr) &this->_$index;
              |}"""
        constructorStmts = constructorStmts :+
          st"Type_assign(&(this->_$index), _$index, sizeof($us $tPtr));"
      }
      accessorImpls = accessorImpls :+ st"$tPtr ${name}_$index($name this);"
    }
    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string_(STACK_FRAME String result, $name this)"
    val header =
      st"""// $tpe
          |$constructorHeader;
          |$eqHeader;
          |$cprintHeader;
          |$stringHeader;
          |
          |${(accessorHeaders, "\n\n")};
          |
          |inline B ${name}__ne($name this, $name other) {
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
          |${(accessorImpls, "\n")}
          |
          |B ${name}__ne($name this, $name other);
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
          |  #ifndef SIREUM_NO_PRINT
          |  String sep = string(", ");
          |  String_cprint(string("("), isOut);
          |  ${cParamTypes(0)}_cprint(${if (isScalar(constructorParamTypes(0)._1)) "" else "&"}this->_1, isOut);
          |  ${
        (
          for (i <- z"1" until size) yield
            st"""String_cprint(sep, isOut);
                |${cParamTypes(i)}_cprint(${if (isScalar(constructorParamTypes(i)._1)) "" else "&"}this->_${i + 1}, isOut);""",
          "\n"
        )
      }
          |  String_cprint(string(")"), isOut);
          |  #endif
          |}
          |
          |$stringHeader {
          |  DeclNewStackFrame(caller, "Tuple$size.scala", "org.sireum.Tuple$size", "string", 0);
          |  String sep = string(", ");
          |  String_string_(SF result, string("("));
          |  ${cParamTypes(0)}_string_(SF result, ${
        if (isScalar(constructorParamTypes(0)._1)) ""
        else s"(${cParamTypes(0).render}) &"
      }this->_1);
          |  ${
        (
          for (i <- z"1" until size)
            yield
              st"""String_string_(SF result, sep);
                  |${cParamTypes(i)}_string_(SF result, ${
                if (isScalar(constructorParamTypes(i)._1)) ""
                else s"(${cParamTypes(i).render}) &"
              }this->_${i + 1});""",
          "\n"
        )
      }
          |  String_string_(SF result, string(")"));
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
      val h = st"$tPtr ${name}_$id(STACK_FRAME_ONLY)"
      globals = globals :+ st"$t _${name}_$id;"
      accessorHeaders = accessorHeaders :+ st"$h;"
      val scalar = isScalar(kind)
      val pre: ST = if (scalar) st"" else st"($tPtr) &"
      accessors = accessors :+
        st"""$h {
            |  ${name}_init(CALLER_LAST);
            |  return ${pre}_${name}_$id;
            |}"""
      if (isVar) {
        val h2 = st"void ${name}_${id}_a(STACK_FRAME $tPtr p_$id)"
        accessorHeaders = accessorHeaders :+ st"$h2;"
        if (scalar) {
          accessors = accessors :+
            st"""$h2 {
                |  ${name}_init(CALLER_LAST);
                |  _${name}_$id = p_$id;
                |}"""
        } else {
          accessors = accessors :+
            st"""$h2 {
                |  ${name}_init(CALLER_LAST);
                |  Type_assign(&_${name}_$id, p_$id, sizeof($t));
                |}"""
        }
      }
    }
    val header =
      st"""void ${name}_init(STACK_FRAME_ONLY);
          |
          |${(accessorHeaders, "\n")}"""
    val impl =
      st"""B ${name}_initialized_ = F;
          |
          |${(globals, "\n")}
          |
          |void ${name}_init(STACK_FRAME_ONLY) {
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

  @pure def localId(id: String): ST = {
    return if (keywords.contains(id)) st"l_$id" else AST.Util.encodeId(id)
  }

  @pure def methodId(id: String): ST = {
    if (keywords.contains(id)) {
      return st"m_$id"
    } else {
      return AST.Util.encodeId(id)
    }
  }

  @pure def fieldId(id: String): ST = {
    return if (keywords.contains(id)) st"f_$id" else AST.Util.encodeId(id)
  }

  @pure def enumId(id: String): ST = {
    return if (keywords.contains(id)) st"E_$id" else AST.Util.encodeId(id)
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
