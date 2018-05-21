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
import org.sireum.transpiler.c.StaticTranspiler.NumberConversionsExtMethodTranspilerPlugin
import org.sireum.transpilers.common.TypeSpecializer

class StaticTranspilerRcTest extends TestSuite {

  lazy val typeChecker: TypeChecker = FrontEnd.checkedLibraryReporter._1
  val dir: Path = Path(implicitly[sourcecode.File].value) / up / up / up / up / up / up / up / up / up / 'app
  def map: scala.collection.Map[scala.Seq[Predef.String], Predef.String] =
    RC.text( Seq("../../../..")) { (p, f) =>
      val filename = p.last
      if (filename.endsWith(".scala")) {
        val r = _root_.java.nio.file.Files.newBufferedReader(f.toPath, _root_.java.nio.charset.StandardCharsets.UTF_8)
        val line: Predef.String = r.readLine
        r.close()
        line != null && line.replaceAllLiterally(" ", "").contains("#Sireum")
      } else false
    }

  val tests = Tests {

    * - testApp("SHA3.scala", T, F)

    //* - testApp("SHA3.scala", F, T)

  }

  def testApp(uri: Predef.String, lineNumber: B, forLoopOpt: B)(
    implicit line: sourcecode.Line
  ): Unit = {
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

    val config = StaticTranspiler.Config(
      projectName = "sha3",
      lineNumber = lineNumber,
      fprintWidth = 3,
      defaultBitWidth = 64,
      maxStringSize = 256,
      maxArraySize = 256,
      customArraySizes = HashMap ++ ISZ(AST.Typed.Name(AST.Typed.isName, ISZ(AST.Typed.z, AST.Typed.string)) ~> 24),
      extMethodTranspilerPlugins = ISZ(NumberConversionsExtMethodTranspilerPlugin()),
      exts = ISZ(),
      forLoopOpt = forLoopOpt
    )

    PostTipeAttrChecker.checkNameTypeMaps(th.nameMap, th.typeMap, reporter)
    if (reporter.hasIssue) {
      reporter.printMessages()
      assert(F)
    }

    val ts = TypeSpecializer.specialize(th, ISZ(TypeSpecializer.EntryPoint.App(ISZ("SHA3"))), reporter)

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

    println()
    println("Running make ...")
    %('make)(resultDir)
  }
}
