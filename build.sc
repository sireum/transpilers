import $file.runtime.Runtime
import $file.slang.Slang
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

object transpilers extends mill.Module {

  final override def millSourcePath = super.millSourcePath / up

  object c extends Transpilers.Module.C {
    override val frontEndObject = slang.frontend
  }

}
