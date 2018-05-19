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
import org.sireum.transpilers.common.TypeSpecializer

class StaticTranspilerTest extends TestSuite {

  lazy val typeChecker: TypeChecker = LibraryTypeCheckingTest.tc
  val dir: Path = Path(implicitly[sourcecode.File].value) / up / up / up / up / up / up / up / up / up / 'result

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

    * - testWorksheet("""import org.sireum.U8._
                        |import org.sireum.N._
                        |val num = u8"10"
                        |println(num)
                        |println(n"1000131384384")""".stripMargin)

    * - testWorksheet("""@enum object Direction {
                        |  'Left
                        |  'Right
                        |}
                        |
                        |println(Direction.Left)
                        |println(Direction.Right)
                        |//println(Direction.byName("Left"))
                        |//println(Direction.byName("Right"))
                        |//println(Direction.byName(""))
                        |//println(Direction.byOrdinal(0))
                        |//println(Direction.byOrdinal(1))
                        |//println(Direction.byOrdinal(2))
                        |println(Direction.elements)
                        |println(Direction.numOfElements)""".stripMargin)

    * - testWorksheet("""val b = T
                        |val n = 4
                        |val p = (b, n)
                        |val t = (p, !p._1 ~> (p._2 + 1), !(!p._1) ~> (p._2 + 2))
                        |println(p)
                        |println(p._1)
                        |println(p._2)
                        |println(t)
                        |println(t._1)
                        |println(t._1._1)
                        |println(t._1._2)
                        |println(t._2)
                        |println(t._2._1)
                        |println(t._2._2)
                        |println(t._3)
                        |println(t._3._1)
                        |println(t._3._2)""".stripMargin)

    * - testWorksheet("""val s = ISZ("a", "b", "c")
                        |println(s)
                        |println(s(0))
                        |println(s(1))
                        |println(s(2))
                        |val s2 = ISZ(s, ISZ("hello"))
                        |println(s2)
                        |println(s2(0)(1))
                        |println(s2(1)(0))
                        |println(s2(0)(1 ~> "d"))
                        |val s3 = ZS.create(5, 1)
                        |println(s3)
                        |println(s3(0 ~> 2, 4 ~> 10))""".stripMargin)
  }

  def testWorksheet(input: Predef.String)(implicit line: sourcecode.Line): Unit = {
    val reporter = Reporter.create
    val (th, p): (TypeHierarchy, TopUnit.Program) =
      Parser(s"import org.sireum._\n$input")
        .parseTopUnit[TopUnit.Program](allowSireum = F, isWorksheet = T, isDiet = F, None(), reporter) match {
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
      projectName = "main",
      lineNumber = T,
      fprintWidth = 3,
      defaultBitWidth = 64,
      maxStringSize = 500,
      maxArraySize = 100,
      customArraySizes = HashMap.empty
    )

    val ts = TypeSpecializer.specialize(th, ISZ(TypeSpecializer.EntryPoint.Worksheet(p)), reporter)

    val trans = StaticTranspiler(config, ts, reporter)

    val r = trans.transpile()

    if (trans.reporter.hasIssue) {
      trans.reporter.printMessages()
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

    println()
    println(s"Running ${config.projectName} ...")
    %(s"./${config.projectName}")(resultDir)
  }
}
