// #Sireum
/*
 Copyright (c) 2017-2022, Robby, Kansas State University
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

package org.sireum.transpilers

import org.sireum._
import org.sireum.cli.CliOpt._

object cli {

  val cTranspiler: Tool = Tool(
    name = "cTranspiler",
    command = "c",
    description = "Slang Embedded to C transpiler",
    header = "Slang Embedded To C Transpiler",
    usage = "<option>* ( <slang-file> )*",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "sourcepath", longKey = "sourcepath", shortKey = Some('s'),
        tpe = Type.Path(T, None()),
        description = "Sourcepath of Slang .scala files"),
      Opt(name = "strictAliasing", longKey = "strict-aliasing", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Enable strict aliasing check"),
      Opt(name = "output", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(F, Some("out")),
        description = "Output directory for transpiled files"),
      Opt(name = "verbose", longKey = "verbose", shortKey = None(),
        tpe = Type.Flag(F), description = "Enable verbose mode")
    ),
    groups = ISZ(
      OptGroup(name = "Configuration", opts = ISZ(
        Opt(name = "apps", longKey = "apps", shortKey = Some('a'),
          tpe = Type.Str(Some(','), None()),
          description = "@app fully qualified names"),
        Opt(name = "bitWidth", longKey = "bits", shortKey = Some('b'),
          tpe = Type.NumChoice(None(), ISZ(64, 32, 16, 8)),
          description = "Default bit-width for unbounded integer types (e.g., Z)"),
        Opt(name = "projectName", longKey = "name", shortKey = Some('n'),
          tpe = Type.Str(None(), Some("main")),
          description = "Project name"),
        Opt(name = "stackSize", longKey = "stack-size", shortKey = Some('z'),
          tpe = Type.Str(None(), Some("16 * 1024 * 1024")),
          description = "Maximum stack size in bytes"),
      )),
      OptGroup(name = "Array Bounds", opts = ISZ(
        Opt(name = "customArraySizes", longKey = "sequence", shortKey = Some('q'),
          tpe = Type.Str(Some(';'), None()),
          description = "Custom maximum sequence sizes, each in the form of <type>=<size>, where <type> is either IS[,], MS[,], ISZ[], MSZ[], or ZS with fully qualified index and element types where applicable"),
        Opt(name = "maxArraySize", longKey = "sequence-size", shortKey = None(),
          tpe = Type.Num(None(), 100, None(), None()),
          description = "Default maximum sequence size"),
        Opt(name = "maxStringSize", longKey = "string-size", shortKey = None(),
          tpe = Type.Num(None(), 100, None(), None()),
          description = "Maximum string size"),
      )),
      OptGroup(name = "CMake", opts = ISZ(
        Opt(name = "cmakeIncludes", longKey = "cmake-includes", shortKey = None(),
          tpe = Type.Path(T, None()),
          description = "Files to embed in generated CMakeLists.txt (can optionally use a preceeding - or + to indicate before/after library/app definitions; defaults to -)"),
        Opt(name = "exts", longKey = "exts", shortKey = Some('e'),
          tpe = Type.Path(T, None()),
          description = "Extension directory or file paths"),
        Opt(name = "libOnly", longKey = "lib-only", shortKey = Some('l'),
          tpe = Type.Flag(F),
          description = "Only generate library definition in CMake file"),
        Opt(name = "excludeBuild", longKey = "exclude-build", shortKey = Some('x'),
          tpe = Type.Str(Some(','), None()),
          description = "Type/method fully qualified names to exclude in the generated CMake file"),
      )),
      OptGroup(name = "Extensibility", opts = ISZ(
        Opt(name = "plugins", longKey = "plugins", shortKey = Some('p'),
          tpe = Type.Str(Some(','), None()),
          description = "Plugin fully qualified names"),
      )),
      OptGroup(name = "Name Mangling", opts = ISZ(
        Opt(name = "fingerprint", longKey = "fingerprint", shortKey = Some('f'),
          tpe = Type.Num(None(), 3, Some(1), Some(64)),
          description = "Generic entity fingerprinting size"),
        Opt(name = "stableTypeId", longKey = "stable-type-id", shortKey = Some('i'),
          tpe = Type.Flag(F),
          description = "Enable stable type id"),
      )),
      OptGroup(name = "Optimizations", opts = ISZ(
        Opt(name = "unroll", longKey = "unroll", shortKey = Some('u'),
          tpe = Type.Flag(F),
          description = "Enable for-loop unrolling on constant bounds"),
      )),
      OptGroup(name = "Persistence", opts = ISZ(
        Opt(name = "save", longKey = "save", shortKey = None(), tpe = Type.Path(F, None()),
          description = "Path to save type information to (outline should not be enabled)"),
        Opt(name = "load", longKey = "load", shortKey = None(), tpe = Type.Path(F, None()),
          description = "Path to load type information from")
      )),
      OptGroup(name = "Substitutions", opts = ISZ(
        Opt(name = "customConstants", longKey = "constants", shortKey = Some('c'),
          tpe = Type.Str(Some(';'), None()),
          description = "Custom constant for object variables, each in the form of <name>=<lit>, where <name> is a qualified name of an object var and <lit> is a Slang literal expression"),
        Opt(name = "forwarding", longKey = "forward", shortKey = Some('w'),
          tpe = Type.Str(Some(','), None()),
          description = "Object forwarding, each in form of <name>=<name>, where <name> is a fully qualified name of an object"),
      )),
    )
  )

  val group: Group = Group(
    name = "transpilers",
    description = "Slang transpilers",
    header = "Slang Transpilers",
    unlisted = F,
    subs = ISZ(cTranspiler)
  )

}