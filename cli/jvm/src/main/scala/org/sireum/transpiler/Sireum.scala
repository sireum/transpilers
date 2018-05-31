package org.sireum.transpiler

import _root_.java.io._
import org.sireum._

object Sireum extends scala.App {
  System.exit(Cli(File.pathSeparatorChar).parseSireum(ISZ(args.toSeq.map(s => s: String):_ *), 0) match {
    case Some(o: Cli.CTranspilerOption) => CTranspiler.run(o)
    case Some(_: Cli.HelpOption) => 0
    case _ => -1
  })
}
