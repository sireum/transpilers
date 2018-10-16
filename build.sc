import $file.runtime.Runtime
import $file.slang.Slang
import $file.alir.Alir
import $file.Transpilers
import ammonite.ops.up

object runtime extends mill.Module {

  object macros extends Runtime.Module.Macros

  object test extends Runtime.Module.Test {
    override def macrosObject = macros
  }

  trait testProvider extends Runtime.Module.TestProvider {
    override def testObject = test
  }

  object library extends Runtime.Module.Library with testProvider {
    override def macrosObject = macros
  }

}

object slang extends mill.Module {

  object ast extends Slang.Module.Ast with runtime.testProvider {
    final override def libraryObject = runtime.library
  }

  object parser extends Slang.Module.Parser with runtime.testProvider {
    final override def astObject = ast
  }

  object tipe extends Slang.Module.Tipe with runtime.testProvider {
    final override def astObject = ast
    final override def testObject = runtime.test
  }

  object frontend extends Slang.Module.FrontEnd with runtime.testProvider {
    final override def parserObject = parser
    final override def tipeObject = tipe
  }

}

object alir extends Alir.Module with runtime.testProvider {
  final override def frontEndObject = slang.frontend
}

object transpilers extends mill.Module {

  final override val millSourcePath = super.millSourcePath / up

  object common extends Transpilers.Module.Common {
    override val alirObject = alir
  }

  object c extends Transpilers.Module.C {
    override val commonObject = common
  }

  object cli extends Transpilers.Module.Cli {
    override val cObject = c
  }

}
