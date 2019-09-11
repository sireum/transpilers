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

import org.sireum.$internal.RC
import org.sireum._
import org.sireum.lang.parser.Parser
import org.sireum.lang.tipe.{PostTipeAttrChecker, TypeChecker, TypeHierarchy}
import org.sireum.lang.{FrontEnd, ast => AST}
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.transpilers.c.StaticTranspiler.{NumberConversionsExtMethodPlugin, StringConversionsExtMethodPlugin}
import org.sireum.transpilers.common.TypeSpecializer

class StaticTranspilerRcTest extends TestSuite {

  lazy val typeChecker: TypeChecker = FrontEnd.checkedLibraryReporter._1
  val dir: Os.Path = Os.path(implicitly[sourcecode.File].value).up.up.up.up.up.up.up.up.up / "app"

  def map: scala.collection.Map[scala.Vector[Predef.String], Predef.String] =
    RC.text(Vector("../../../..")) { (p, f) =>
      val filename = p.last
      if (filename.endsWith(".scala")) {
        val r = _root_.java.nio.file.Files.newBufferedReader(f.toPath, _root_.java.nio.charset.StandardCharsets.UTF_8)
        val line: Predef.String = r.readLine
        r.close()
        line != null && line.replaceAllLiterally(" ", "").contains("#Sireum")
      } else filename.endsWith(".c") || filename.endsWith(".h")
    }

  val tests = Tests {

    * - testApp("sha3", "sha3.scala", F)

    * - testApp("sha3-unrolled", "sha3.scala", T)

  }

  def testApp(name: String, uri: Predef.String, forLoopOpt: B)(implicit line: sourcecode.Line): Unit = {
    (dir / name.value).removeAll()

    val reporter = Reporter.create
    val (th, p): (TypeHierarchy, AST.TopUnit.Program) =
      Parser(map(Vector(uri)))
        .parseTopUnit[AST.TopUnit.Program](allowSireum = F, isWorksheet = T, isDiet = F, Some(uri), reporter) match {
        case Some(program) if !reporter.hasIssue =>
          val p = FrontEnd.checkWorksheet(Some(typeChecker.typeHierarchy), program, reporter)
          if (reporter.hasIssue) {
            reporter.printMessages()
            assert(F)
          }
          p
        case _ =>
          reporter.printMessages()
          assert(F)
          halt(())
      }

    val extKey = Vector("ext", "ext.c")
    val extFile = StaticTranspiler.ExtFile(extKey.mkString("/"), map(extKey))

    val config = StaticTranspiler.Config(
      projectName = "sha3",
      fprintWidth = 3,
      defaultBitWidth = 64,
      maxStringSize = 256,
      maxArraySize = 256,
      customArraySizes = HashMap ++ ISZ(
        AST.Typed.Name(AST.Typed.isName, ISZ(AST.Typed.z, AST.Typed.string)) ~> 2
      ),
      customConstants = HashMap.empty,
      plugins = ISZ(
        NumberConversionsExtMethodPlugin(),
        StringConversionsExtMethodPlugin()
      ),
      exts = ISZ(extFile),
      excludedNames = HashSet.empty[ISZ[String]],
      forLoopOpt = forLoopOpt,
      stackSize = "16 * 1024 * 1024",
      libOnly = F
    )

    PostTipeAttrChecker.checkNameTypeMaps(th.nameMap, th.typeMap, reporter)
    if (reporter.hasIssue) {
      reporter.printMessages()
      assert(F)
    }

    val ts = TypeSpecializer.specialize(th, ISZ(TypeSpecializer.EntryPoint.App(ISZ("sha3"))), HashMap.empty, reporter)

    val trans = StaticTranspiler(config, ts)

    val r = trans.transpile(reporter)

    reporter.printMessages()
    if (reporter.hasIssue) {
      assert(F)
    }

    val resultDir = dir / s"L${line.value}"
    resultDir.removeAll()
    resultDir.mkdirAll()

    for (e <- r.files.entries) {
      val path = e._1
      var f = resultDir
      for (segment <- path) {
        f = f / segment.value
      }
      f.up.mkdirAll()
      f = f.canon
      f.writeOver(e._2.render)
      println(s"Wrote $f")
    }
    for (e <- r.extFiles.entries) {
      val path = e._1
      val extFile = e._2
      var f = resultDir
      for (segment <- path) {
        f = f / segment.value
      }
      f.up.mkdirAll()
      f = f.canon
      f.writeOver(extFile.content)
      println(s"Wrote $f")
    }

    println()
    println("Running CMake ...")
    Os.proc(ISZ("cmake", "-DCMAKE_BUILD_TYPE=Release", /* "-DWITH_LOC=on", */ ".")).at(resultDir).console.runCheck()

    val ldir = dir / s"L${line.value}"

    try {
      println()
      println("Running make ...")
      Os.proc(ISZ("make")).at(resultDir).console.runCheck()
    } finally {
      (ldir / "CMakeFiles").removeAll()
      (ldir / "cmake_install.cmake").removeAll()
      (ldir / "CMakeCache.txt").removeAll()
    }

    if (Os.isWin) {
      (ldir / "sha3.exe").moveOverTo(dir / s"${name.value}.exe")
    } else {
      (ldir / "sha3").moveOverTo(dir / name.value)
    }
  }
}
