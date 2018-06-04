import org.sireum._
import org.sireum.transpiler.{CTranspiler, Cli}
import java.io._

import ammonite.ops._

object BuildingControlGenPeriodicApp extends scala.App {
  if (args.length != 2) {
    println("Usage: BuildingControlApp <slang-embedded-path> <output-path>")
  } else {
    BuildingControlApp.transpile(args, "building-control-gen-periodic")
  }
}

object BuildingControlGenApp extends scala.App {
  if (args.length != 2) {
    println("Usage: BuildingControlApp <slang-embedded-path> <output-path>")
  } else {
    BuildingControlApp.transpile(args, "building-control-gen")
  }
}

object BuildingControlApp {

  def transpile(args: Array[Predef.String], example: Predef.String): Unit = {
    val slangPath = Path(new File(args(0)).getCanonicalFile.getAbsoluteFile)
    val out = Path(new File(args(1)).getCanonicalFile.getAbsoluteFile) / example
    val dir = Path(new File(implicitly[sourcecode.File].value).getParentFile)
    val pkg = example.replaceAllLiterally("-", "_")
    val extFile = dir / up / 'c / pkg / "ext.c"
    val readme = dir / pkg / "readme.md"

    rm ! out

    mkdir ! out / 'src
    cp(slangPath / example / 'src / 'aadl, out / 'src / 'aadl)
    cp(slangPath / example / 'src / 'main, out / 'src / 'scala)
    cp(slangPath / 'art / 'src / 'main / 'scala / 'art, out / 'src / 'scala / 'art)
    cp(readme, out / "readme.md")

    write(out / 'bin / "compile-mac.sh", """#!/usr/bin/env bash
                                           |set -e
                                           |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                           |cd $SCRIPT_HOME
                                           |mkdir -p mac
                                           |mkdir -p $SCRIPT_HOME/../src/c/mac
                                           |cd $SCRIPT_HOME/../src/c/mac
                                           |cmake -DCMAKE_BUILD_TYPE=Release ..
                                           |make $MAKE_ARGS
                                           |mv *_App $SCRIPT_HOME/mac/
                                           |mv *_AEP $SCRIPT_HOME/mac/
                                           |mv Main $SCRIPT_HOME/mac/""".stripMargin)

    write(out / 'bin / "compile-linux.sh", """#!/usr/bin/env bash
                                             |set -e
                                             |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                             |cd $SCRIPT_HOME
                                             |mkdir -p linux
                                             |mkdir -p $SCRIPT_HOME/../src/c/linux
                                             |cd $SCRIPT_HOME/../src/c/linux
                                             |cmake -DCMAKE_BUILD_TYPE=Release ..
                                             |make $MAKE_ARGS
                                             |mv *_App $SCRIPT_HOME/linux/
                                             |mv *_AEP $SCRIPT_HOME/linux/
                                             |mv Main $SCRIPT_HOME/linux/""".stripMargin)

    write(out / 'bin / "compile-cygwin.sh", """#!/usr/bin/env bash
                                              |set -e
                                              |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                              |cd $SCRIPT_HOME
                                              |mkdir -p win
                                              |mkdir -p $SCRIPT_HOME/../src/c/win
                                              |cd $SCRIPT_HOME/../src/c/win
                                              |cmake -DCMAKE_BUILD_TYPE=Release ..
                                              |make $MAKE_ARGS
                                              |mv *.exe $SCRIPT_HOME/win/""".stripMargin)

    write(out / 'bin / "run-mac.sh", """#!/usr/bin/env bash
                                       |set -e
                                       |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                       |cd $SCRIPT_HOME
                                       |mac/TempControl_i_AEP 2> /dev/null &
                                       |mac/Fan_i_AEP 2> /dev/null &
                                       |open -a Terminal mac/TempControl_i_App
                                       |open -a Terminal mac/TempSensor_i_App
                                       |open -a Terminal mac/Fan_i_App
                                       |read -p "Press enter to start ..."
                                       |mac/Main""".stripMargin)

    write(out / 'bin / "run-linux.sh", """#!/usr/bin/env bash
                                         |set -e
                                         |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                         |cd $SCRIPT_HOME
                                         |linux/TempControl_i_AEP 2> /dev/null &
                                         |linux/Fan_i_AEP 2> /dev/null &
                                         |x-terminal-emulator -e sh -c linux/TempControl_i_App &
                                         |x-terminal-emulator -e sh -c linux/TempSensor_i_App &
                                         |x-terminal-emulator -e sh -c linux/Fan_i_App &
                                         |read -p "Press enter to start ..."
                                         |linux/Main""".stripMargin)

    write(out / 'bin / "run-cygwin.sh", """#!/usr/bin/env bash
                                          |set -e
                                          |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                          |cd $SCRIPT_HOME
                                          |win/TempControl_i_AEP.exe 2> /dev/null &
                                          |win/Fan_i_AEP.exe 2> /dev/null &
                                          |cygstart mintty /bin/bash -c win/TempControl_i_App.exe &
                                          |cygstart mintty /bin/bash -c win/TempSensor_i_App.exe &
                                          |cygstart mintty /bin/bash -c win/Fan_i_App.exe &
                                          |read -p "Press enter to start ..."
                                          |win/Main.exe""".stripMargin)

    write(out / 'bin / "stop.sh", """#!/usr/bin/env bash
                                    |APPS="TempControl TempSensor Fan"
                                    |for APP in ${APPS}; do
                                    |  pkill $APP
                                    |  pkill -9 $APP
                                    |done
                                    |ME=`whoami`
                                    |IPCS_Q=`ipcs -q | egrep "[0-9a-f]+[0-9]+" | grep $ME | cut -f2 -d" "`
                                    |for id in $IPCS_Q; do
                                    |  ipcrm -q $id;
                                    |done
                                    |ipcs""".stripMargin)

    for (f <- (out / 'bin).toIO.listFiles((_, name) => name.endsWith(".sh"))) {
      %('chmod, "+x", 'bin / f.getName)(out)
    }

    Cli(File.pathSeparatorChar).parseSireum(
      ISZ(
        "transpiler",
        "c",
        "--verbose",
        "--sourcepath",
        s"${out / 'src / 'scala}",
        "--bits",
        "32",
        "--string-size",
        "256",
        "--sequence-size",
        "16",
        "--sequence",
        "ISZ[org.sireumString]=2",
        "--apps",
        s"$pkg.Fan_i_AEP,$pkg.Fan_i_App,$pkg.TempControl_i_AEP,$pkg.TempControl_i_App,$pkg.TempSensor_i_App,$pkg.Main",
        "--forward",
        s"art.ArtNative=$pkg.ArtNix,$pkg.Platform=$pkg.PlatformNix",
        "--exts",
        extFile.toString,
        "--output-dir",
        (out / 'src / 'c).toString
      ),
      0
    ) match {
      case Some(o: Cli.CTranspilerOption) =>
        val baos = new ByteArrayOutputStream()
        val sysOut = System.out
        val sysErr = System.err
        val outs = new OutputStream {
          override def write(b: Int): Unit = {
            sysOut.write(b)
            baos.write(b)
          }
        }
        val errs = new OutputStream {
          override def write(b: Int): Unit = {
            sysErr.write(b)
            baos.write(b)
          }
        }
        System.setOut(new PrintStream(outs))
        System.setErr(new PrintStream(errs))
        CTranspiler.run(o)
        val s = new java.lang.String(baos.toByteArray)
        write(out / "transpiler.log", s)
      case Some(_: Cli.HelpOption) => sys.exit(0)
      case _ => sys.exit(-1)
    }

    val zipFilename = s"${out.segments.last}.zip"
    rm ! out / up / zipFilename
    %('zip, "-qro", zipFilename, out.segments.last)(out / up)
  }

}
