package org.sireum.transpilers.c

import ammonite.ops._
import org.sireum.$internal.RC
import org.sireum._
import org.sireum.lang.parser.Parser
import org.sireum.lang.tipe.{PostTipeAttrChecker, TypeChecker, TypeHierarchy}
import org.sireum.lang.{FrontEnd, ast => AST}
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.transpiler.c.StaticTranspiler
import org.sireum.transpiler.c.StaticTranspiler.{NumberConversionsExtMethodTranspilerPlugin, StringConversionsExtMethodTranspilerPlugin}
import org.sireum.transpilers.common.TypeSpecializer

class StaticTranspilerRcTest extends TestSuite {

  lazy val typeChecker: TypeChecker = FrontEnd.checkedLibraryReporter._1
  val dir: Path = Path(implicitly[sourcecode.File].value) / up / up / up / up / up / up / up / up / up / 'app

  def map: scala.collection.Map[scala.Seq[Predef.String], Predef.String] =
    RC.text(Seq("../../../..")) { (p, f) =>
      val filename = p.last
      if (filename.endsWith(".scala")) {
        val r = _root_.java.nio.file.Files.newBufferedReader(f.toPath, _root_.java.nio.charset.StandardCharsets.UTF_8)
        val line: Predef.String = r.readLine
        r.close()
        line != null && line.replaceAllLiterally(" ", "").contains("#Sireum")
      } else filename.endsWith(".c") || filename.endsWith(".h")
    }

  val tests = Tests {

    * - testApp("sha3", "sha3.scala", T, F)

    * - testApp("sha3-unrolled", "sha3.scala", F, T)

  }

  def testApp(name: String, uri: Predef.String, lineNumber: B, forLoopOpt: B)(implicit line: sourcecode.Line): Unit = {
    rm! dir / name.value

    val reporter = Reporter.create
    val (th, p): (TypeHierarchy, AST.TopUnit.Program) =
      Parser(map(Seq(uri)))
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

    val extKey = Seq("ext", "ext.c")
    val extFile = StaticTranspiler.ExtFile(extKey.mkString("/"), map(extKey))

    val config = StaticTranspiler.Config(
      projectName = "sha3",
      lineNumber = lineNumber,
      fprintWidth = 3,
      defaultBitWidth = 64,
      maxStringSize = 256,
      maxArraySize = 256,
      customArraySizes = HashMap ++ ISZ(AST.Typed.Name(AST.Typed.isName, ISZ(AST.Typed.z, AST.Typed.string)) ~> 24),
      extMethodTranspilerPlugins = ISZ(
        NumberConversionsExtMethodTranspilerPlugin(),
        StringConversionsExtMethodTranspilerPlugin()
      ),
      exts = ISZ(extFile),
      forLoopOpt = forLoopOpt
    )

    PostTipeAttrChecker.checkNameTypeMaps(th.nameMap, th.typeMap, reporter)
    if (reporter.hasIssue) {
      reporter.printMessages()
      assert(F)
    }

    val ts = TypeSpecializer.specialize(th, ISZ(TypeSpecializer.EntryPoint.App(ISZ("sha3"))), reporter)

    val trans = StaticTranspiler(config, ts, reporter)

    val r = trans.transpile()

    trans.reporter.printMessages()
    if (trans.reporter.hasIssue) {
      assert(F)
    }

    val resultDir = dir / s"L${line.value}"
    rm ! resultDir
    mkdir ! resultDir

    for (e <- r.files.entries) {
      val path = e._1
      var f = resultDir
      for (segment <- path) {
        f = f / segment.value
      }
      mkdir ! f / up
      write.over(f, e._2.render.value)
      println(s"Wrote $f")
    }

    println()
    println("Running CMake ...")
    %('cmake, "-DCMAKE_BUILD_TYPE=Release", ".")(resultDir)

    val ldir = dir / s"L${line.value}"
    rm ! ldir / 'CMakeFiles
    rm ! ldir / "cmake_install.cmake"
    rm ! ldir / "CMakeCache.txt"

    println()
    println("Running make ...")
    %('make)(resultDir)

    mv(ldir / 'main, dir / name.value)
  }
}
