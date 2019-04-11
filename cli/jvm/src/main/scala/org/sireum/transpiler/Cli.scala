// #Sireum
// @formatter:off

/*
 Copyright (c) 2018, Robby, Kansas State University
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

// This file is auto-generated from cli.sc

package org.sireum.transpiler

import org.sireum._

object Cli {

  @datatype trait SireumTopOption

  @datatype class HelpOption extends SireumTopOption

  @datatype class CTranspilerOption(
    help: String,
    args: ISZ[String],
    sourcepath: ISZ[String],
    output: Option[String],
    verbose: B,
    projectName: Option[String],
    apps: ISZ[String],
    unroll: B,
    fingerprint: Z,
    bitWidth: Z,
    maxStringSize: Z,
    maxArraySize: Z,
    customArraySizes: ISZ[String],
    customConstants: ISZ[String],
    plugins: ISZ[String],
    exts: ISZ[String],
    forwarding: ISZ[String],
    save: Option[String],
    load: Option[String]
  ) extends SireumTopOption
}

import Cli._

@record class Cli(pathSep: C) {

  def parseSireum(args: ISZ[String], i: Z): Option[SireumTopOption] = {
    if (i >= args.size) {
      println(
        st"""Sireum: A High-Assurance Software Development Platform
            |(c) 2018, SAnToS Laboratory, Kansas State University
            |
            |Available modes:
            |transpiler               Slang transpilers""".render
      )
      return Some(HelpOption())
    }
    val opt = select("sireum", args, i, ISZ("transpiler"))
    opt match {
      case Some(string"transpiler") => parseTranspiler(args, i + 1)
      case _ => return None()
    }
  }

  def parseTranspiler(args: ISZ[String], i: Z): Option[SireumTopOption] = {
    if (i >= args.size) {
      println(
        st"""The Sireum Language (Slang) Transpilers
            |
            |Available modes:
            |c                        Slang to C transpiler""".render
      )
      return Some(HelpOption())
    }
    val opt = select("transpiler", args, i, ISZ("c"))
    opt match {
      case Some(string"c") => parseCTranspiler(args, i + 1)
      case _ => return None()
    }
  }

  def parseCTranspiler(args: ISZ[String], i: Z): Option[SireumTopOption] = {
    val help =
      st"""Slang To C Transpiler
          |
          |Usage: <option>* ( <slang-file> )*
          |
          |Available Options:
          |-s, --sourcepath         Sourcepath of Slang .scala files (expects path
          |                           strings)
          |-o, --output-dir         Output directory for transpiled files (expects a path;
          |                           default is "out")
          |    --verbose            Enable verbose mode
          |-h, --help               Display this information
          |
          |Configuration Options:
          |-n, --name               Project name (expects a string)
          |-a, --apps               @app fully qualified names (expects a string separated
          |                           by ",")
          |-u, --unroll             Enable for-loop unrolling
          |-f, --fingerprint        Generic entity fingerprinting size (expects an integer;
          |                           default is 3)
          |-b, --bits               Default bit-width for unbounded integer types (e.g.,
          |                           Z) (expects one of { 64, 32, 16, 8 })
          |    --string-size        Maximum string size (expects an integer; default is
          |                           100)
          |    --sequence-size      Default maximum sequence size (expects an integer;
          |                           default is 100)
          |-q, --sequence           Custom maximum sequence sizes, each in the form of
          |                           <type>=<size>, where <type> is either IS[,], MS[,],
          |                           ISZ[], MSZ[], or ZS with fully qualified index and
          |                           element types where applicable (expects a string
          |                           separated by ",")
          |-c, --constants          Custom constant for object variables, each in the form
          |                           of <name>=<lit>, where <name> is a qualified name of
          |                           an object var and <lit> is a Slang literal
          |                           expression (expects a string separated by ",")
          |-p, --plugins            Plugin fully qualified names (expects a string
          |                           separated by ",")
          |-e, --exts               Extension file paths (expects path strings)
          |-w, --forward            Object forwarding, each in form of <name>=<name>,
          |                           where <name> is a fully qualified name of an object
          |                           (expects a string separated by ",")
          |
          |Persistence Options:
          |    --save               Path to save type information to (outline should not
          |                           be enabled) (expects a path)
          |    --load               Path to load type information from (expects a path)""".render

    var sourcepath: ISZ[String] = ISZ[String]()
    var output: Option[String] = Some("out")
    var verbose: B = false
    var projectName: Option[String] = Some("main")
    var apps: ISZ[String] = ISZ[String]()
    var unroll: B = false
    var fingerprint: Z = 3
    var bitWidth: Z = 64
    var maxStringSize: Z = 100
    var maxArraySize: Z = 100
    var customArraySizes: ISZ[String] = ISZ[String]()
    var customConstants: ISZ[String] = ISZ[String]()
    var plugins: ISZ[String] = ISZ[String]()
    var exts: ISZ[String] = ISZ[String]()
    var forwarding: ISZ[String] = ISZ[String]()
    var save: Option[String] = None[String]()
    var load: Option[String] = None[String]()
    var j = i
    var isOption = T
    while (j < args.size && isOption) {
      val arg = args(j)
      if (ops.StringOps(arg).first == '-') {
        if (args(j) == "-h" || args(j) == "--help") {
          println(help)
          return Some(HelpOption())
        } else if (arg == "-s" || arg == "--sourcepath") {
           val o: Option[ISZ[String]] = parsePaths(args, j + 1)
           o match {
             case Some(v) => sourcepath = v
             case _ => return None()
           }
         } else if (arg == "-o" || arg == "--output-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => output = v
             case _ => return None()
           }
         } else if (arg == "--verbose") {
           val o: Option[B] = { j = j - 1; Some(!verbose) }
           o match {
             case Some(v) => verbose = v
             case _ => return None()
           }
         } else if (arg == "-n" || arg == "--name") {
           val o: Option[Option[String]] = parseString(args, j + 1)
           o match {
             case Some(v) => projectName = v
             case _ => return None()
           }
         } else if (arg == "-a" || arg == "--apps") {
           val o: Option[ISZ[String]] = parseStrings(args, j + 1, ',')
           o match {
             case Some(v) => apps = v
             case _ => return None()
           }
         } else if (arg == "-u" || arg == "--unroll") {
           val o: Option[B] = { j = j - 1; Some(!unroll) }
           o match {
             case Some(v) => unroll = v
             case _ => return None()
           }
         } else if (arg == "-f" || arg == "--fingerprint") {
           val o: Option[Z] = parseNum(args, j + 1, Some(1), Some(64))
           o match {
             case Some(v) => fingerprint = v
             case _ => return None()
           }
         } else if (arg == "-b" || arg == "--bits") {
           val o: Option[Z] = parseNumChoice(args, j + 1, ISZ(z"64", z"32", z"16", z"8"))
           o match {
             case Some(v) => bitWidth = v
             case _ => return None()
           }
         } else if (arg == "--string-size") {
           val o: Option[Z] = parseNum(args, j + 1, None(), None())
           o match {
             case Some(v) => maxStringSize = v
             case _ => return None()
           }
         } else if (arg == "--sequence-size") {
           val o: Option[Z] = parseNum(args, j + 1, None(), None())
           o match {
             case Some(v) => maxArraySize = v
             case _ => return None()
           }
         } else if (arg == "-q" || arg == "--sequence") {
           val o: Option[ISZ[String]] = parseStrings(args, j + 1, ',')
           o match {
             case Some(v) => customArraySizes = v
             case _ => return None()
           }
         } else if (arg == "-c" || arg == "--constants") {
           val o: Option[ISZ[String]] = parseStrings(args, j + 1, ',')
           o match {
             case Some(v) => customConstants = v
             case _ => return None()
           }
         } else if (arg == "-p" || arg == "--plugins") {
           val o: Option[ISZ[String]] = parseStrings(args, j + 1, ',')
           o match {
             case Some(v) => plugins = v
             case _ => return None()
           }
         } else if (arg == "-e" || arg == "--exts") {
           val o: Option[ISZ[String]] = parsePaths(args, j + 1)
           o match {
             case Some(v) => exts = v
             case _ => return None()
           }
         } else if (arg == "-w" || arg == "--forward") {
           val o: Option[ISZ[String]] = parseStrings(args, j + 1, ',')
           o match {
             case Some(v) => forwarding = v
             case _ => return None()
           }
         } else if (arg == "--save") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => save = v
             case _ => return None()
           }
         } else if (arg == "--load") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => load = v
             case _ => return None()
           }
         } else {
          eprintln(s"Unrecognized option '$arg'.")
          return None()
        }
        j = j + 2
      } else {
        isOption = F
      }
    }
    return Some(CTranspilerOption(help, parseArguments(args, j), sourcepath, output, verbose, projectName, apps, unroll, fingerprint, bitWidth, maxStringSize, maxArraySize, customArraySizes, customConstants, plugins, exts, forwarding, save, load))
  }

  def parseArguments(args: ISZ[String], i: Z): ISZ[String] = {
    var r = ISZ[String]()
    var j = i
    while (j < args.size) {
      r = r :+ args(j)
      j = j + 1
    }
    return r
  }

  def parsePaths(args: ISZ[String], i: Z): Option[ISZ[String]] = {
    return tokenize(args, i, "path", pathSep, F)
  }

  def parsePath(args: ISZ[String], i: Z): Option[Option[String]] = {
    if (i >= args.size) {
      eprintln("Expecting a path, but none found.")
    }
    return Some(Some(args(i)))
  }

  def parseStrings(args: ISZ[String], i: Z, sep: C): Option[ISZ[String]] = {
    tokenize(args, i, "string", sep, F) match {
      case r@Some(_) => return r
      case _ => return None()
    }
  }

  def parseString(args: ISZ[String], i: Z): Option[Option[String]] = {
    if (i >= args.size) {
      eprintln("Expecting a string, but none found.")
      return None()
    }
    return Some(Some(args(i)))
  }

  def parseNums(args: ISZ[String], i: Z, sep: C, minOpt: Option[Z], maxOpt: Option[Z]): Option[ISZ[Z]] = {
    tokenize(args, i, "integer", sep, T) match {
      case Some(sargs) =>
        var r = ISZ[Z]()
        for (arg <- sargs) {
          parseNumH(arg, minOpt, maxOpt) match {
            case Some(n) => r = r :+ n
            case _ => return None()
          }
        }
        return Some(r)
      case _ => return None()
    }
  }

  def tokenize(args: ISZ[String], i: Z, tpe: String, sep: C, removeWhitespace: B): Option[ISZ[String]] = {
    if (i >= args.size) {
      eprintln(s"Expecting a sequence of $tpe separated by '$sep', but none found.")
      return None()
    }
    val arg = args(i)
    return Some(tokenizeH(arg, sep, removeWhitespace))
  }

  def tokenizeH(arg: String, sep: C, removeWhitespace: B): ISZ[String] = {
    val argCis = conversions.String.toCis(arg)
    var r = ISZ[String]()
    var cis = ISZ[C]()
    var j = 0
    while (j < argCis.size) {
      val c = argCis(j)
      if (c == sep) {
        r = r :+ conversions.String.fromCis(cis)
        cis = ISZ[C]()
      } else {
        val allowed: B = c match {
          case c"\n" => !removeWhitespace
          case c" " => !removeWhitespace
          case c"\r" => !removeWhitespace
          case c"\t" => !removeWhitespace
          case _ => T
        }
        if (allowed) {
          cis = cis :+ c
        }
      }
      j = j + 1
    }
    if (cis.size > 0) {
      r = r :+ conversions.String.fromCis(cis)
    }
    return r
  }

  def parseNumChoice(args: ISZ[String], i: Z, choices: ISZ[Z]): Option[Z] = {
    val set = HashSet.empty[Z] ++ choices
    parseNum(args, i, None(), None()) match {
      case r@Some(n) =>
        if (set.contains(n)) {
          return r
        } else {
          eprintln(s"Expecting one of the following: $set, but found $n.")
          return None()
        }
      case r => return r
    }
  }

  def parseNum(args: ISZ[String], i: Z, minOpt: Option[Z], maxOpt: Option[Z]): Option[Z] = {
    if (i >= args.size) {
      eprintln(s"Expecting an integer, but none found.")
      return None()
    }
    return parseNumH(args(i), minOpt, maxOpt)
  }

  def parseNumH(arg: String, minOpt: Option[Z], maxOpt: Option[Z]): Option[Z] = {
    Z(arg) match {
      case Some(n) =>
        minOpt match {
          case Some(min) =>
            if (n < min) {
              eprintln(s"Expecting an integer at least $min, but found $n.")
              return None()
            }
          case _ =>
        }
        maxOpt match {
          case Some(max) =>
            if (n > max) {
              eprintln(s"Expecting an integer at most $max, but found $n.")
              return None()
            }
            return Some(n)
          case _ =>
        }
        return Some(n)
      case _ =>
        eprintln(s"Expecting an integer, but found '$arg'.")
        return None()
    }
  }

  def select(mode: String, args: ISZ[String], i: Z, choices: ISZ[String]): Option[String] = {
    val arg = args(i)
    var cs = ISZ[String]()
    for (c <- choices) {
      if (ops.StringOps(c).startsWith(arg)) {
        cs = cs :+ c
      }
    }
    cs.size match {
      case z"0" =>
        eprintln(s"$arg is not a mode of $mode.")
        return None()
      case z"1" => return Some(cs(0))
      case _ =>
        eprintln(
          st"""Which one of the following modes did you mean by '$arg'?
              |${(cs, "\n")}""".render)
        return None()
    }
  }
}