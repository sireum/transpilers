package org.sireum.transpilers.c

import org.sireum._
import org.sireum.lang.{FrontEnd, LibraryTypeCheckingTest}
import org.sireum.lang.ast.TopUnit
import org.sireum.lang.parser.Parser
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.transpiler.c.StaticTranspiler
import ammonite.ops._

class StaticTranspilerTest extends TestSuite {

  lazy val typeChecker: TypeChecker = LibraryTypeCheckingTest.tc
  val dir: Path = Path(implicitly[sourcecode.File].value) / up / 'result

  val tests = Tests {

    * - testWorksheet("""println("Hello World!")""".stripMargin)

    * - testWorksheet("""val x = 5 * 5 + 1
                        |assert(x == 26)
                        |println(x)""".stripMargin)

    * - testWorksheet("""val x = 5
                        |var y = F
                        |assume(!y)
                        |if (y && x < 6) {
                        |  println(x)
                        |  println(y)
                        |} else {
                        |  println(y)
                        |  println(x)
                        |}""".stripMargin)

    * - testWorksheet("""var i = 0
                        |while (i < 6) {
                        |  println(i)
                        |  i = i + 1
                        |}""".stripMargin)

  }

  def testWorksheet(input: Predef.String)(implicit line: sourcecode.Line): Boolean = {
    val reporter = Reporter.create
    val (th, p): (TypeHierarchy, TopUnit.Program) =
      Parser(s"import org.sireum._\n$input")
        .parseTopUnit[TopUnit.Program](allowSireum = F, isWorksheet = T, isDiet = F, None(), reporter) match {
        case Some(program) if !reporter.hasIssue =>
          val p = FrontEnd.checkWorksheet(Some(typeChecker.typeHierarchy), program, reporter)
          if (reporter.hasIssue) {
            reporter.printMessages()
            return false
          }
          p
        case _ =>
          reporter.printMessages()
          return false
      }

    val config = StaticTranspiler.Config(
      exeName = "main",
      lineNumber = T,
      fprintWidth = 3,
      defaultBitWidth = 64,
      defaultStringSize = 512,
      defaultArraySize = 512,
      customArraySizes = HashMap.empty
    )

    val trans = StaticTranspiler(config, th, reporter)

    val r = trans.transpileWorksheet(p)

    if (trans.reporter.hasIssue) {
      trans.reporter.printMessages()
      return false
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

    println()
    println(s"Running program ${config.exeName} ...")
    %(s"./${config.exeName}")(resultDir)

    return true
  }
}
