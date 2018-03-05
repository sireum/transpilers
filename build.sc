import $file.runtime.Runtime
import $file.slang.Slang
import $file.tools.Tools
import $file.Transpilers
import ammonite.ops.up

object runtime extends mill.Module {

  object macros extends Runtime.Module.Macros

  object library extends Runtime.Module.Library {

    final override def macrosObject = macros

  }

}

object slang extends mill.Module {

  object ast extends Slang.Module.Ast {

    override val libraryObject = runtime.library

  }

  object parser extends Slang.Module.Parser {

    final override val astObject = ast

  }

  object tipe extends Slang.Module.Tipe {

    final override def astObject = ast

  }

  object frontend extends Slang.Module.FrontEnd {

    final override def parserObject = parser

    final override def tipeObject = tipe

  }

}

object tools extends Tools.Module {

  override val frontEndObject = slang.frontend

}

object transpilers extends mill.Module {

  final override def millSourcePath = super.millSourcePath / up

  object common extends Transpilers.Module.Common {
    override val frontEndObject = slang.frontend
  }

  object c extends Transpilers.Module.C {
    override val commonObject = common
  }

  object cli extends Transpilers.Module.Cli {
    override val cObject = c
    override val toolsObject = tools
  }

}
