// #Sireum
/*
 Copyright (c) 2019, Robby, Kansas State University
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

  @pure def typeCompositeH(stringMax: Z, isStringMax: Z, typeNames: ISZ[(String, ST, ST)]): ST = {
    val r =
      st"""#ifndef SIREUM_GEN_TYPE_H
          |#define SIREUM_GEN_TYPE_H
          |
          |#include <memory.h>
          |#include <stackframe.h>
          |
          |typedef enum {
          |  T${typeNames(0)._2} = 0, // ${typeNames(0)._1}
          |  ${
        (
          for (tn <- ops.ISZOps(ops.ISZOps(typeNames).zip(typeNames.indices.map((n: Z) => n))).drop(1))
            yield st"T${tn._1._2} = ${tn._2}, // ${tn._1._1}",
          "\n"
        )
      }
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
          |#define string(v) ((String) &((struct { TYPE type; Z size; C value[sizeof(v)]; }) { TString, Z_C(sizeof (v) - 1), v }))
          |#define DeclNewString(x) struct StaticString x = { .type = TString }
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
          |inline size_t sizeOf(Type t) {
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
          |extern size_t sizeOf(Type t);
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

  @pure def allH(names: ISZ[QName], entries: ISZ[ST]): ST = {
    val r =
      st"""#ifndef SIREUM_ALL_H
          |#define SIREUM_ALL_H
          |
          |#include <types.h>
          |${(for (name <- ops.ISZOps(names).sortWith(qnameLt _)) yield st"#include <${(name, "_")}.h>", "\n")}
          |
          |${(entries, "\n")}
          |
          |#endif"""
    return r
  }

  @pure def allC(typeNames: ISZ[(String, ST, ST)], entries: ISZ[ST]): ST = {
    val r =
      st"""#include <all.h>
          |#include <errno.h>
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
          |void Type_string(STACK_FRAME String result, void *this) {
          |  TYPE type = ((Type) this)->type;
          |  switch (type) {
          |    ${
        (
          for (tn <- typeNames) yield st"case T${tn._2}: ${tn._2}_string(CALLER result, (${tn._2}) this); return;",
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

  @pure def cmake(project: String, stackSize: String, mainFilenames: ISZ[ISZ[String]], filess: ISZ[QName]): ST = {
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
            |target_link_libraries($name-bin LINK_PUBLIC $project)
            |
            |set_target_properties($name-bin PROPERTIES OUTPUT_NAME $name)"""
      return r
    }

    val mains: ISZ[ST] = for (f <- mainFilenames) yield target(f)

    val r =
      st"""cmake_minimum_required(VERSION 3.6.2)
          |
          |project($project)
          |
          |set(CMAKE_C_STANDARD 99)
          |
          |add_compile_options(-Werror)
          |
          |add_compile_options("$$<$$<CONFIG:Release>:-O2>")
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
          |add_library($project STATIC
          |        ${(files(filess), "\n")})
          |
          |target_include_directories($project
          |        ${(for (d <- includeDirs(filess)) yield st"PUBLIC $d", "\n")})
          |
          |${(mains, "\n\n")}
          |
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
          |to_hex("$stackSize" stack_size)
          |if ("$${CMAKE_CXX_COMPILER_ID}" MATCHES "(C|c?)lang")
          |  set(CMAKE_EXE_LINKER_FLAGS "-Wl,-stack_size -Wl,$${stack_size}")
          |elseif ("$${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
          |  if (WIN32 OR MINGW OR MSYS OR CYGWIN)
          |    set(CMAKE_EXE_LINKER_FLAGS "-Wl,--stack,$${stack_size}")
          |  endif()
          |endif()"""
    return r
  }

  @pure def typeManglingMap(entries: ISZ[(String, String)]): ST = {
    val sortedEntries = ops.ISZOps(entries).sortWith((p1, p2) => p1._1 < p2._1)
    return st"${(for (p <- sortedEntries) yield st"${p._1}=${p._2}", "\n")}"
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
            |#include <misc.h>
            |
            |${(comp.typeHeader, "\n\n")}
            |
            |#endif"""))
      r = r :+ ((dir :+ headerFilename,
        st"""#ifndef SIREUM_H_$filename
            |#define SIREUM_H_$filename
            |#include <types.h>
            |
            |${(comp.header, "\n\n")}
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
    val r: ST = if (atExit.nonEmpty) {
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
          |  return (int) ${mangleName(owner)}_$id(SF &t_args);
          |}"""
    } else {
      st"""#include <all.h>
          |#include <string.h>
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
          |  return (int) ${mangleName(owner)}_$id(SF &t_args);
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
          |#define DeclNew$name(x) struct $name x = { .type = T$name }"""

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
          accessors = accessors :+ st"#define ${name}_${vd.id}_a(this, value) Type_assign(&(this)->${vd.id}, value, sizeof(${vd.tpe}))"
        }
      }
    }

    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val neHeader =
      st"""inline B ${name}__ne($name this, $name other) {
          |  return !${name}__eq(this, other);
          |}"""
    val neImpl = st"extern B ${name}__ne($name this, $name other);"
    val stringHeader = st"void ${name}_string(STACK_FRAME String result, $name this)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"

    val header =
      st"""// $tpe
          |
          |${(accessors, "\n")}
          |
          |$eqHeader;
          |$neHeader;
          |$stringHeader;
          |$cprintHeader;
          |
          |inline B ${name}__is(STACK_FRAME void* this) {
          |  return (($name) this)->type == T$name;
          |}
          |
          |inline $name ${name}__as(STACK_FRAME void *this) {
          |  if (${name}__is(CALLER this)) return ($name) this;
          |  fprintf(stderr, "Invalid case from %s to $tpe.", TYPE_string(this));
          |  sfAbortImpl(CALLER "");
          |  $abort
          |}"""

    var eqStmts = ISZ[ST]()
    var stringStmts = ISZ[ST](
      st"""DeclNewStackFrame(caller, "$uri", "${dotName(className)}", "string", 0);
          |String_string(SF result, string("${className(className.size - 1)}("));""")
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
        stringStmts = stringStmts :+ st"String_string(SF result, sep);"
        cprintStmts = cprintStmts :+ st"String_cprint(sep, isOut);"
      }
      stringStmts = stringStmts :+ st"${vd.tpePtr}_string(SF result, ${pre}this->${vd.id});"
      cprintStmts = cprintStmts :+ st"${vd.tpePtr}_cprint(${pre}this->${vd.id}, isOut);"
    }
    stringStmts = stringStmts :+ st"""String_string(SF result, string(")"));"""
    cprintStmts = cprintStmts :+ st"""String_cprint(string(")"), isOut);"""

    val impl =
      st"""// $tpe
          |
          |$eqHeader {
          |  ${(eqStmts, "\n")}
          |  return T;
          |}
          |
          |$neImpl
          |
          |$stringHeader {
          |  ${(stringStmts, "\n")}
          |}
          |
          |$cprintHeader {
          |  #ifndef SIREUM_NO_PRINT
          |  ${(cprintStmts, "\n")}
          |  #endif
          |}
          |
          |extern B ${name}__is(STACK_FRAME void* this);
          |extern $name ${name}__as(STACK_FRAME void *this);"""

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
          |inline void ${name}_string(STACK_FRAME String result, $name this) {
          |  Type_string(CALLER result, this);
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
          |      fprintf(stderr, "Invalid cast from %s to $tpe.", TYPE_string(this));
          |      sfAbortImpl(CALLER "");
          |  }
          |  return ($name) this;
          |}
          |
          |extern void ${name}_string(STACK_FRAME String result, $name this);"""
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
    val atH = st"$elementTypePtr ${name}_at($name this, $indexType i)"
    val upH = st"void ${name}_up($name this, $indexType i, $elementTypePtr e)"
    val atImpl = st"extern $atH;"
    val upImpl = st"extern $upH;"
    val (atHeader, upHeader): (ST, ST) =
      if (isElementTypeScalar)
        (
          st"""inline $atH {
              |  return (this)->value[($sizeType) (i)$offset];
              |}""",
          st"""inline $upH {
              |  (this)->value[($sizeType) (i)$offset] = e;
              |}""")
      else
        (
          st"""inline $atH {
              |  return ($elementTypePtr) &((this)->value[($sizeType) (i)$offset]);
              |}""",
          st"""inline $upH {
              |  Type_assign(&(this)->value[($sizeType) (i)$offset], e, sizeof($elementType));
              |}""")
    val (toOtherHeaderOpt, toOtherImplOpt): (Option[ST], Option[ST]) = otherNameOpt match {
      case Some(otherName) =>
        val other: String = if (isImmutable) "MS" else "IS"
        (Some(
          st"""
              |inline void ${name}_to$other(STACK_FRAME $otherName result, $name this) {
              |  STATIC_ASSERT(Max$otherName >= Max$name, "Invalid cast from $tpe to $other[...,...].");
              |  result->type = T$otherName;
              |  result->size = this->size;
              |  memcpy(&result->value, &this->value, this->size * sizeof($elementType));
              |}"""), Some(
          st"""
              |extern void ${name}_to$other(STACK_FRAME $otherName result, $name this);"""))
      case _ => (None(), None())
    }
    val typeHeader =
      st"""// $tpe
          |${(includes, "\n")}
          |
          |#define Max$name $maxElement
          |#define ${name}SizeT $sizeType
          |
          |typedef struct $name *$name;
          |struct $name {
          |  TYPE type;
          |  $sizeType size;
          |  $elementType value[Max$name];
          |};
          |
          |#define DeclNew$name(x) struct $name x = { .type = T$name }"""
    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val createHeader = st"void ${name}_create(STACK_FRAME $name result, $indexType size, $elementTypePtr dflt)"
    val zreateHeader = st"void ${name}_zreate(STACK_FRAME $name result, Z size, $elementTypePtr dflt)"
    val appendHeader = st"void ${name}__append(STACK_FRAME $name result, $name this, $elementTypePtr value)"
    val prependHeader = st"void ${name}__prepend(STACK_FRAME $name result, $name this, $elementTypePtr value)"
    val appendAllHeader = st"void ${name}__appendAll(STACK_FRAME $name result, $name this, $name other)"
    val removeHeader = st"void ${name}__remove(STACK_FRAME $name result, $name this, $elementTypePtr value)"
    val removeAllHeader = st"void ${name}__removeAll(STACK_FRAME $name result, $name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string(STACK_FRAME String result, $name this)"
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
      if (isElementTypeScalar)
        st"""// $tpe
            |$atImpl
            |$upImpl
            |extern $indexType ${name}_size(STACK_FRAME $name this);
            |extern Z ${name}_zize(STACK_FRAME $name this);
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
            |  Type_assign(result, this, sizeof($name));
            |  for ($sizeType i = 0; i < otherSize; i++)
            |    result->value[thisSize + i] = other->value[i];
            |  result->size = ($sizeType) (thisSize + otherSize);
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
            |  #endif
            |}
            |
            |$stringHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
            |  String_string(SF result, string("["));
            |  $sizeType size = this->size;
            |  if (size > 0) {
            |    $elementType *value = this->value;
            |    String space = string(" ");
            |    String_string(SF result, space);
            |    ${elementTypePtr}_string(SF result, value[0]);
            |    for ($sizeType i = 1; i < size; i++) {
            |      String_string(SF result, string(", "));
            |      ${elementTypePtr}_string(SF result, value[i]);
            |    }
            |    String_string(SF result, space);
            |  }
            |  String_string(SF result, string("]"));
            |}
            |
            |extern B ${name}__ne($name this, $name other);
            |$toOtherImplOpt"""
      else
        st"""// $tpe
            |$atImpl
            |$upImpl
            |extern $indexType ${name}_size(STACK_FRAME $name this);
            |extern Z ${name}_zize(STACK_FRAME $name this);
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
            |  for ($sizeType i = 0; i < otherSize; i++)
            |    Type_assign(&result->value[thisSize + i], &other->value[i], sizeof($elementType));
            |  result->size = ($sizeType) thisSize + otherSize;
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
            |  #endif
            |}
            |
            |$stringHeader {
            |  DeclNewStackFrame(caller, "$sName.scala", "org.sireum.$sName", "string", 0);
            |  String_string(SF result, string("["));
            |  $sizeType size = this->size;
            |  if (size > 0) {
            |    $elementType *value = this->value;
            |    String space = string(" ");
            |    String_string(SF result, space);
            |    ${elementTypePtr}_string(SF result, ($elementTypePtr) &(value[0]));
            |    for ($sizeType i = 1; i < size; i++) {
            |      String_string(SF result, string(", "));
            |      ${elementTypePtr}_string(SF result, ($elementTypePtr) &(value[i]));
            |    }
            |    String_string(SF result, space);
            |  }
            |  String_string(SF result, string("]"));
            |}
            |
            |extern B ${name}__ne($name this, $name other);
            |$toOtherImplOpt"""
    return compiled(
      typeHeader = compiled.typeHeader :+ typeHeader,
      header = compiled.header :+ header,
      impl = compiled.impl :+ impl
    )
  }

  @pure def elementName(owner: QName, id: String): ST = {
    return st"${mangleName(owner)}_${enumId(id)}"
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
          |inline void ${mangledName}_name(String result, $mangledName this) {
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

    val stringHeader = st"void ${mangledName}_string(STACK_FRAME String result, $mangledName this)"
    header = header :+ stringHeader
    impl = impl :+
      st"""$stringHeader {
          |  DeclNewStackFrame(caller, "$uri", "$mangledName", "string", 0);
          |  switch (this) {
          |    ${
        (
          for (e <- elements) yield st"""case ${elemName(e)}: String_string(SF result, string("$e")); return;""",
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
            |extern B ${mangledName}__eq($mangledName this, $mangledName other);
            |extern B ${mangledName}__ne($mangledName this, $mangledName other);
            |extern Z ${mangledName}__ordinal($mangledName this);
            |extern void ${mangledName}_name(String result, $mangledName this);
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
    val min: ST = minOpt match {
      case Some(m) => st"${mangledName}_C($m)"
      case _ => st"${cTypeUp}_MIN"
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
              |extern $mangledName ${mangledName}__complement($mangledName n);
              |extern $mangledName ${mangledName}__shl($mangledName n1, $mangledName n2);
              |extern $mangledName ${mangledName}__shr($mangledName n1, $mangledName n2);
              |extern $mangledName ${mangledName}__ushr($mangledName n1, $mangledName n2);
              |extern $mangledName ${mangledName}__and($mangledName n1, $mangledName n2);
              |extern $mangledName ${mangledName}__or($mangledName n1, $mangledName n2);
              |extern $mangledName ${mangledName}__xor($mangledName n1, $mangledName n2);"""
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
          |#define ${mangledName}_Min ${cTypeUp}_MIN
          |#define ${mangledName}_Max ${cTypeUp}_MAX
          |
          |#define ${mangledName}_F "%$hex" $pr """""
    val stringHeader = st"void ${mangledName}_string(STACK_FRAME String result, $mangledName this)"
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
      impl = impl :+ st"extern $mangledName ${mangledName}_range($mangledName n);"
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
          |  return (B) (n1 == n2);
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
      st"""extern $mangledName ${mangledName}__plus($mangledName n);
          |extern $mangledName ${mangledName}__minus($mangledName n);
          |extern $mangledName ${mangledName}__add($mangledName n1, $mangledName n2);
          |extern $mangledName ${mangledName}__sub($mangledName n1, $mangledName n2);
          |extern $mangledName ${mangledName}__mul($mangledName n1, $mangledName n2);
          |extern $mangledName ${mangledName}__div($mangledName n1, $mangledName n2);
          |extern $mangledName ${mangledName}__rem($mangledName n1, $mangledName n2);
          |extern B ${mangledName}__eq($mangledName n1, $mangledName n2);
          |extern B ${mangledName}__ne($mangledName n1, $mangledName n2);
          |extern B ${mangledName}__lt($mangledName n1, $mangledName n2);
          |extern B ${mangledName}__le($mangledName n1, $mangledName n2);
          |extern B ${mangledName}__gt($mangledName n1, $mangledName n2);
          |extern B ${mangledName}__ge($mangledName n1, $mangledName n2);
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
        st"""#include <assert.h>
            |
            |${(header, ";\n")};""",
      impl = compiled.impl :+
        st"""#include <errno.h>
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
    val mangledName = mangleName(name)
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
      accessorImpls = accessorImpls :+ st"extern $tPtr ${name}_$index($name this);"
    }
    val eqHeader = st"B ${name}__eq($name this, $name other)"
    val cprintHeader = st"void ${name}_cprint($name this, B isOut)"
    val stringHeader = st"void ${name}_string(STACK_FRAME String result, $name this)"
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
          |extern B ${name}__ne($name this, $name other);
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
          |  String_string(SF result, string("("));
          |  ${cParamTypes(0)}_string(SF result, ${
        if (isScalar(constructorParamTypes(0)._1)) ""
        else s"(${cParamTypes(0).render}) &"
      }this->_1);
          |  ${
        (
          for (i <- z"1" until size)
            yield
              st"""String_string(SF result, sep);
                  |${cParamTypes(i)}_string(SF result, ${
                if (isScalar(constructorParamTypes(i)._1)) ""
                else s"(${cParamTypes(i).render}) &"
              }this->_${i + 1});""",
          "\n"
        )
      }
          |  String_string(SF result, string(")"));
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
      val h = st"$tPtr ${name}_$id(STACK_FRAME_LAST)"
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
      st"""void ${name}_init(STACK_FRAME_LAST);
          |
          |${(accessorHeaders, "\n")}"""
    val impl =
      st"""B ${name}_initialized_ = F;
          |
          |${(globals, "\n")}
          |
          |void ${name}_init(STACK_FRAME_LAST) {
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
    return if (keywords.contains(id)) st"l_$id" else encodeName(id)
  }

  @pure def methodId(id: String): ST = {
    if (keywords.contains(id)) {
      return st"m_$id"
    } else {
      id match {
        case AST.Exp.BinaryOp.Add => return st"_add"
        case AST.Exp.BinaryOp.Sub => return st"_sub"
        case AST.Exp.BinaryOp.Mul => return st"_mul"
        case AST.Exp.BinaryOp.Div => return st"_div"
        case AST.Exp.BinaryOp.Rem => return st"_rem"
        case AST.Exp.BinaryOp.Eq => return st"_eq"
        case AST.Exp.BinaryOp.Ne => return st"_ne"
        case AST.Exp.BinaryOp.Shl => return st"_lt"
        case AST.Exp.BinaryOp.Shr => return st"_le"
        case AST.Exp.BinaryOp.Ushr => return st"_gt"
        case AST.Exp.BinaryOp.Lt => return st"_ge"
        case AST.Exp.BinaryOp.Le => return st"_shl"
        case AST.Exp.BinaryOp.Gt => return st"_shr"
        case AST.Exp.BinaryOp.Ge => return st"_ushr"
        case AST.Exp.BinaryOp.And => return st"_and"
        case AST.Exp.BinaryOp.Or => return st"_or"
        case AST.Exp.BinaryOp.Xor => return st"_xor"
        case AST.Exp.BinaryOp.Append => return st"_append"
        case AST.Exp.BinaryOp.Prepend => return st"_prepend"
        case AST.Exp.BinaryOp.AppendAll => return st"_appendall"
        case AST.Exp.BinaryOp.RemoveAll => return st"_removeall"
        case _ => return encodeName(id)
      }
    }
  }

  @pure def fieldId(id: String): ST = {
    return if (keywords.contains(id)) st"f_$id" else encodeName(id)
  }

  @pure def enumId(id: String): ST = {
    return if (keywords.contains(id)) st"E_$id" else encodeName(id)
  }

  @pure def mangleName(ids: QName): ST = {
    val r: ST =
      ids.size match {
        case z"0" => st"top"
        case z"1" => st"top_${ids(0)}"
        case _ => st"${(AST.Typed.short(ids).map(encodeName _), "_")}"
      }
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
