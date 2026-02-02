/*
 Copyright (c) 2017-2026,Robby, Kansas State University
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
import org.sireum.lang.FrontEnd
import org.sireum.lang.{ast => AST}
import org.sireum.lang.parser.Parser
import org.sireum.lang.tipe.{PostTipeAttrChecker, TypeChecker, TypeHierarchy}
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.transpilers.c.StaticTranspiler.NumberConversionsExtMethodPlugin
import org.sireum.alir.TypeSpecializer

class StaticTranspilerTest extends TestSuite {

  lazy val typeChecker: TypeChecker = FrontEnd.checkedLibraryReporter._1
  val dir: Os.Path = Os.path(implicitly[sourcecode.File].value).up.up.up.up.up.up.up.up.up / "result"

  val tests = Tests {

    * - testWorksheet(
      """@datatype class Foo(val x: Z, val y: Z)
        |
        |{
        |  val foo1 = Foo(3, 5)
        |  println(foo1)
        |}
        |
        |println(Foo(1, 6).x)
        |
        |{
        |  val foo2 = Foo(2, 4)
        |  println(foo2)
        |}""".stripMargin)

    * - testWorksheet(
      """@sig trait Foo {
        |  def foo(): Unit = {
        |    bar()
        |  }
        |  def bar(): Unit
        |}
        |
        |@sig trait Bar extends Foo {
        |  def bar(): Unit = {
        |  }
        |}
        |
        |@datatype class Baz extends Bar
        |
        |Baz().foo()""".stripMargin)

    * - testWorksheet("""println("Foo\nBar")""".stripMargin)

    * - testWorksheet("""import org.sireum.N8._
                        |import org.sireum.S8._
                        |import org.sireum.S64._
                        |import org.sireum.U8._
                        |import org.sireum.U16._
                        |import org.sireum.U32._
                        |import org.sireum.U64._
                        |assert(C.fromZ('A'.toZ) == 'A')
                        |assert(n8"4".toZ == 4)
                        |assert(N8.fromZ(n8"4".toZ) == n8"4")
                        |assert(S8.fromZ(s8"-1".toZ) == s8"-1")
                        |assert(S64.fromZ(s64"-1".toZ) == s64"-1")
                        |assert(U64.fromZ(u32"1".toZ) == u64"1")
                        |assert(U8.fromZ(u16"1".toZ) == u8"1")""".stripMargin)

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
                        |println(Direction.Left.name)
                        |println(Direction.Right.name)
                        |println(Direction.byName("Left"))
                        |println(Direction.byName("Right"))
                        |println(Direction.byName("Left").get)
                        |println(Direction.byName("Right").get)
                        |println(Direction.byName(""))
                        |println(Direction.byOrdinal(0))
                        |println(Direction.byOrdinal(1))
                        |println(Direction.byOrdinal(2))
                        |println(Direction.byOrdinal(0).get)
                        |println(Direction.byOrdinal(1).get)
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
                        |println(s3(0 ~> 2, 4 ~> 10))""".stripMargin, 10, 10)

    * - testWorksheet("""var bs1 = ISZ[B](T, F, T)
                        |println(s"$bs1.size = ${bs1.size}")
                        |bs1 = bs1 :+ T
                        |println(s"$bs1.size = ${bs1.size}")
                        |bs1 = bs1 ++ bs1
                        |println(s"$bs1.size = ${bs1.size}")
                        |bs1 = bs1 - T
                        |println(s"$bs1.size = ${bs1.size}")
                        |bs1 = bs1 -- ISZ(F)
                        |println(s"$bs1.size = ${bs1.size}")
                        |bs1 = IS.create(7, T)
                        |println(s"$bs1.size = ${bs1.size}")
                        |bs1 = F +: bs1
                        |println(s"$bs1.size = ${bs1.size}")
                        |""".stripMargin)

    * - testWorksheet("""def foo(x: Z): Z = {
                        |  return x + 1
                        |}
                        |assert(foo(4) == 5)""".stripMargin)

    * - testWorksheet(
      """val success: B = (1 * 2) match {
        |  case 2 => T
        |  case x => halt(s"Unexpected: '${x}'")
        |}""".stripMargin)

    * - testWorksheet("""import org.sireum.U8._
                        |val hash = crypto.SHA3.sum256(ISZ())
                        |println(hash)
                        |assert(hash == ISZ(
                        |  u8"0xa7", u8"0xff", u8"0xc6", u8"0xf8", u8"0xbf", u8"0x1e", u8"0xd7", u8"0x66",
                        |  u8"0x51", u8"0xc1", u8"0x47", u8"0x56", u8"0xa0", u8"0x61", u8"0xd6", u8"0x62",
                        |  u8"0xf5", u8"0x80", u8"0xff", u8"0x4d", u8"0xe4", u8"0x3b", u8"0x49", u8"0xfa",
                        |  u8"0x82", u8"0xd8", u8"0x0a", u8"0x4b", u8"0x80", u8"0xf8", u8"0x43", u8"0x4a"
                        |))""".stripMargin, 256)

    * - testWorksheet("""import org.sireum.U8._
                        |val hash = crypto.SHA3.sum256(IS.create(200, u8"0xa3"))
                        |println(hash)
                        |assert(hash == ISZ(
                        |  u8"0x79", u8"0xf3", u8"0x8a", u8"0xde", u8"0xc5", u8"0xc2", u8"0x03", u8"0x07",
                        |  u8"0xa9", u8"0x8e", u8"0xf7", u8"0x6e", u8"0x83", u8"0x24", u8"0xaf", u8"0xbf",
                        |  u8"0xd4", u8"0x6c", u8"0xfd", u8"0x81", u8"0xb2", u8"0x2e", u8"0x39", u8"0x73",
                        |  u8"0xc6", u8"0x5f", u8"0xa1", u8"0xbd", u8"0x9d", u8"0xe3", u8"0x17", u8"0x87"
                        |))""".stripMargin, 256)

    * - testWorksheet("""import org.sireum.U8._
                        |val hash = crypto.SHA3.sum384(IS.create(200, u8"0xa3"))
                        |println(hash)
                        |assert(hash == ISZ(
                        |  u8"0x18", u8"0x81", u8"0xde", u8"0x2c", u8"0xa7", u8"0xe4", u8"0x1e", u8"0xf9",
                        |  u8"0x5d", u8"0xc4", u8"0x73", u8"0x2b", u8"0x8f", u8"0x5f", u8"0x00", u8"0x2b",
                        |  u8"0x18", u8"0x9c", u8"0xc1", u8"0xe4", u8"0x2b", u8"0x74", u8"0x16", u8"0x8e",
                        |  u8"0xd1", u8"0x73", u8"0x26", u8"0x49", u8"0xce", u8"0x1d", u8"0xbc", u8"0xdd",
                        |  u8"0x76", u8"0x19", u8"0x7a", u8"0x31", u8"0xfd", u8"0x55", u8"0xee", u8"0x98",
                        |  u8"0x9f", u8"0x2d", u8"0x70", u8"0x50", u8"0xdd", u8"0x47", u8"0x3e", u8"0x8f"
                        |))""".stripMargin, 256)

    * - testWorksheet("""import org.sireum.U8._
                        |val hash = crypto.SHA3.sum512(IS.create(200, u8"0xa3"))
                        |println(hash)
                        |assert(hash == ISZ(
                        |  u8"0xe7", u8"0x6d", u8"0xfa", u8"0xd2", u8"0x20", u8"0x84", u8"0xa8", u8"0xb1",
                        |  u8"0x46", u8"0x7f", u8"0xcf", u8"0x2f", u8"0xfa", u8"0x58", u8"0x36", u8"0x1b",
                        |  u8"0xec", u8"0x76", u8"0x28", u8"0xed", u8"0xf5", u8"0xf3", u8"0xfd", u8"0xc0",
                        |  u8"0xe4", u8"0x80", u8"0x5d", u8"0xc4", u8"0x8c", u8"0xae", u8"0xec", u8"0xa8",
                        |  u8"0x1b", u8"0x7c", u8"0x13", u8"0xc3", u8"0x0a", u8"0xdf", u8"0x52", u8"0xa3",
                        |  u8"0x65", u8"0x95", u8"0x84", u8"0x73", u8"0x9a", u8"0x2d", u8"0xf4", u8"0x6b",
                        |  u8"0xe5", u8"0x89", u8"0xc5", u8"0x1c", u8"0xa1", u8"0xa4", u8"0xa8", u8"0x41",
                        |  u8"0x6d", u8"0xf6", u8"0x54", u8"0x5a", u8"0x1c", u8"0xe8", u8"0xba", u8"0x00"
                        |))""".stripMargin, 256)

    * - testWorksheet("""import org.sireum.U8._
                        |val a3 = ISZ(u8"0xa3")
                        |val sha3 = crypto.SHA3.init512
                        |for (_ <- 0 until 200) {
                        |  sha3.update(a3)
                        |}
                        |val hash = sha3.finalise()
                        |println(hash)
                        |assert(hash == ISZ(
                        |  u8"0xe7", u8"0x6d", u8"0xfa", u8"0xd2", u8"0x20", u8"0x84", u8"0xa8", u8"0xb1",
                        |  u8"0x46", u8"0x7f", u8"0xcf", u8"0x2f", u8"0xfa", u8"0x58", u8"0x36", u8"0x1b",
                        |  u8"0xec", u8"0x76", u8"0x28", u8"0xed", u8"0xf5", u8"0xf3", u8"0xfd", u8"0xc0",
                        |  u8"0xe4", u8"0x80", u8"0x5d", u8"0xc4", u8"0x8c", u8"0xae", u8"0xec", u8"0xa8",
                        |  u8"0x1b", u8"0x7c", u8"0x13", u8"0xc3", u8"0x0a", u8"0xdf", u8"0x52", u8"0xa3",
                        |  u8"0x65", u8"0x95", u8"0x84", u8"0x73", u8"0x9a", u8"0x2d", u8"0xf4", u8"0x6b",
                        |  u8"0xe5", u8"0x89", u8"0xc5", u8"0x1c", u8"0xa1", u8"0xa4", u8"0xa8", u8"0x41",
                        |  u8"0x6d", u8"0xf6", u8"0x54", u8"0x5a", u8"0x1c", u8"0xe8", u8"0xba", u8"0x00"
                        |))""".stripMargin, 256)

    * - testWorksheet("""@datatype class Foo(a: ISZ[Z])
                        |
                        |val foo = Foo(ISZ(1, 2, 3))
                        |val first = foo.a(0)
                        |assert(first == 1)""".stripMargin)

    * - testWorksheet(s"""@datatype class Foo(a: ISZ[Z]) {
                         |  override def string: String = {
                         |    return s"$${a.size}"
                         |  }
                         |}
                         |val foo = Foo(ISZ(1, 2, 3))
                         |println(foo)""".stripMargin)

    * - testWorksheet(s"""@datatype class Foo(x: Z, y: Z)
                         |@datatype class Bar(z: Z, foo: Foo)
                         |object Baz {
                         |  val foo: Foo = Foo(1, 1)
                         |}
                         |val foo = Foo(1, 2)
                         |println(foo(x = 2))
                         |val bar = Bar(3, foo)
                         |println(bar.foo(x = 3))
                         |println(Baz.foo(x = 4))
                         |println(bar(foo = Baz.foo(x = 5)))
                         |val s = MSZ(foo)
                         |s(0) = s(0)(x = 6)
                         |println(s)""".stripMargin)

    * - testWorksheet("""@record trait Foo {
                        |  def x: Z
                        |}
                        |@record class Bar(x: Z) extends Foo
                        |@record class Baz extends Foo {
                        |  def x: Z = {
                        |    return 0
                        |  }
                        |}
                        |val bar: Foo = Bar(4)
                        |println(bar.x)
                        |assert(bar.x == 4)""".stripMargin)

    * - testWorksheet("""@record trait Foo {
                        |  def x: Z
                        |}
                        |@record class Bar(x: Z) extends Foo
                        |@record class Baz extends Foo {
                        |  def x: Z = {
                        |    return 0
                        |  }
                        |}
                        |val baz: Foo = Baz()
                        |println(baz.x)
                        |assert(baz.x == 0)""".stripMargin)

    * - testWorksheet("""val opt: Option[Option[Z]] = Some(Some(4))
                        |val Some(Some(x)) = opt
                        |println(x)""".stripMargin)

    * - testWorksheet("""val x: Z = if (F) 1 else 2
                        |println(x)
                        |val opt: Option[Z] = if (T) Some(5) else None()
                        |println(opt.get)""".stripMargin)

    * - testWorksheet("""val opt: Option[Option[Z]] = Some(Some(1))
                        |val y = 1
                        |opt match {
                        |  case Some(Some(0)) => println("zero")
                        |  case Some(Some(`y`)) => println("y")
                        |  case Some(Some(x)) if x > 4 => println(">4")
                        |  case Some(Some(x)) => println(x)
                        |  case Some(None()) => println("Some(None())")
                        |  case None() => println("None()")
                        |}""".stripMargin)

    * - testWorksheet("""for (n <- ISZ("a", "b", "c")) {
                        |  println(n)
                        |}""".stripMargin, 10, 10)

    * - testWorksheet("""println(for (ss <- ISZ(ISZ("a", "b"), ISZ("c")); s <- ss) yield s"${s}1")""", 10, 10)

    * - testWorksheet("""import org.sireum.N._
                        |println(B("T"))
                        |println(B("F"))
                        |println(B("true"))
                        |println(B("false"))
                        |println(B("5"))
                        |println(C("abc"))
                        |println(C(""))
                        |println(Z("123"))
                        |println(Z("abc"))
                        |println(F32("0"))
                        |println(F32("1.0"))
                        |println(F32("a"))
                        |println(F64("0"))
                        |println(F64("1.0"))
                        |println(F64("a"))
                        |println(R("0"))
                        |println(R("1.0"))
                        |println(R("a"))
                        |println(N("0"))
                        |println(N("-1"))""".stripMargin)

    * - testWorksheet("""@datatype class Foo(a: ISZ[Z])
                        |
                        |val foo = Foo(ISZ(1, 2, 3))
                        |val first = foo.a(0)
                        |assert(first == 1)""".stripMargin)


    * - testWorksheet("""object Foo {
                        |  @pure def foo(x: Z): Z = {
                        |    var r = x
                        |    def foo1(): Unit = {
                        |      def foo2(): Unit = {
                        |        r = r + 1
                        |      }
                        |      foo2()
                        |    }
                        |    foo1()
                        |    return r
                        |  }
                        |  @pure def fooP[T](x: T, y: T): T = {
                        |    var r: Option[T] = Some(x)
                        |    def foo1(): Unit = {
                        |      var r2 = y
                        |      def foo2(): Unit = {
                        |        r = Some(r2)
                        |      }
                        |      foo2()
                        |    }
                        |    foo1()
                        |    return r.get
                        |  }
                        |}
                        |
                        |@datatype class Bar[T](x: T) {
                        |  @pure def bar(y: T): T = {
                        |    var r: Option[T] = Some(x)
                        |    def bar1(): Unit = {
                        |      var r2 = y
                        |      def bar2(): Unit = {
                        |        r = Some(r2)
                        |      }
                        |      bar2()
                        |    }
                        |    bar1()
                        |    return r.get
                        |  }
                        |}
                        |
                        |println(Foo.foo(4))
                        |println(Foo.fooP(4, 5))
                        |println(Bar(4).bar(5))""".stripMargin)

    * - testWorksheet("""val s = MSZ[Option[Option[Z]]](Some(Some(5)), Some(None[Z]()))
                        |s(0) match {
                        |  case Some(o) =>
                        |    s(0) = None()
                        |    assert(o == Some(5))
                        |  case _ =>
                        |}""".stripMargin)

    * - testWorksheet("""var m = Map.empty[Z, Z]
                        |m = m + 1 ~> 2
                        |assert(m.get(1) == Some(2))""".stripMargin)

    * - testWorksheet("""var m = Map.empty[Z, Map[Z, Z]]
                        |m = m + 1 ~> Map.empty[Z, Z]
                        |assert(m.get(1) == Some(Map.empty[Z, Z]))""".stripMargin, 10, 10)

    * - testWorksheet(
      """def foo(opt: Option[Z]): Z = {
        |  opt match {
        |    case Some(x) => return x
        |    case _ => return 0
        |  }
        |}
        |
        |foo(Some(4))""".stripMargin)

    * - testWorksheet(
      """var n = 0
        |for (i <- 0 until 10; j <- 0 until 2) {
        |  n = n + 1
        |}
        |println(n)
        |assert(n == 20)""".stripMargin)

    * - testWorksheet("""object Foo {
                        |  val connections: MS[Z, ISZ[Z]] = MSZ(ISZ(0))
                        |}
                        |
                        |val cs: ISZ[Z] = Foo.connections(0)""".stripMargin, 10, 10)

    * - testWorksheet(
      """val s1 = ISZ(1, 2, 3)
        |val s2 = ISZ(4, 5)
        |val s3 = s1 ++ s2
        |println(s"$s1.size = ${s1.size}")
        |println(s"$s2.size = ${s2.size}")
        |println(s"$s3.size = ${s3.size}")
        |assert(s3 == ISZ(1, 2, 3, 4, 5))""".stripMargin)

    * - testWorksheet(
      """@datatype class Foo(x: Z)
        |val zMopt: MOption[Foo] = MSome(Foo(3))
        |val MSome(foo) = zMopt
        |assert(foo.x == 3)""".stripMargin)

  }

  def testWorksheet(input: Predef.String, arraySize: Z = 100, stringSize: Z = 256)(implicit line: sourcecode.Line): Unit = {
    val reporter = Reporter.create
    val (th, p): (TypeHierarchy, AST.TopUnit.Program) =
      Parser(s"import org.sireum._\n$input")
        .parseTopUnit[AST.TopUnit.Program](isWorksheet = T, isDiet = F, None(), reporter) match {
        case Some(program) if !reporter.hasIssue =>
          val p = FrontEnd.checkWorksheet(0, Some(typeChecker.typeHierarchy), program, reporter)
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
      fprintWidth = 3,
      defaultBitWidth = 64,
      maxStringSize = stringSize,
      maxArraySize = arraySize,
      customArraySizes = HashMap.empty,
      customConstants = HashMap.empty,
      plugins = ISZ(NumberConversionsExtMethodPlugin()),
      exts = ISZ(),
      excludedNames = HashSet.empty,
      forLoopOpt = F,
      stackSize = "16 * 1024 * 1024",
      libOnly = F,
      stableTypeId = F,
      cmakeIncludes = ISZ(),
      cmakePlusIncludes = ISZ()
    )

    PostTipeAttrChecker.checkNameTypeMaps(th.nameMap, th.typeMap, reporter)
    if (reporter.hasIssue) {
      reporter.printMessages()
      assert(F)
    }

    val ts = TypeSpecializer.specialize(th, ISZ(TypeSpecializer.EntryPoint.Worksheet(p)), HashMap.empty, reporter)

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
      f.writeOver(e._2.render.value)
      println(s"Wrote $f")
    }

    println()
    println("Running CMake ...")
    Os.proc(ISZ("cmake", "-DCMAKE_BUILD_TYPE=Release", "-DWITH_LOC=on", "-DRANGE_CHECK=on", ".")).at(resultDir).echo.console.runCheck()

    val ldir = dir / s"L${line.value}"

    try {
      println()
      println("Running make ...")
      Os.proc(ISZ("make")).at(resultDir).console.runCheck()

      println()
      println(s"Running ${config.projectName} ...")
      Os.proc(ISZ((resultDir / config.projectName).string)).at(resultDir).console.runCheck()
    } finally {
      (ldir / "CMakeFiles").removeAll()
      (ldir / "cmake_install.cmake").removeAll()
      (ldir / "CMakeCache.txt").removeAll()
    }
  }
}
