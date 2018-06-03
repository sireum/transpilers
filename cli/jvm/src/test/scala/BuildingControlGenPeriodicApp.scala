import org.sireum._
import org.sireum.transpiler.{CTranspiler, Cli}
import java.io._

import ammonite.ops._

object BuildingControlGenPeriodicApp extends scala.App {

  if (args.length != 2) {
    println("Usage: BuildingControlGenPeriodicApp <slang-embedded-path> <output-path>")
  } else {

    val slangPath = Path(new File(args(0)).getCanonicalFile.getAbsoluteFile)
    val out = Path(new File(args(1)).getCanonicalFile.getAbsoluteFile)

    rm ! out

    mkdir ! out / 'src
    cp(slangPath / "building-control-gen-periodic" / 'src / 'main, out / 'src / 'scala)
    cp(slangPath / 'art / 'src / 'main / 'scala / 'art, out / 'src / 'scala / 'art)

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
                                         |xterm -e sh -c linux/TempControl_i_App &
                                         |xterm -e sh -c linux/TempSensor_i_App &
                                         |xterm -e sh -c linux/Fan_i_App &
                                         |read -p "Press enter to start ..."
                                         |linux/Main""".stripMargin)
    write(
      out / 'bin / "stop.sh",
      """#!/usr/bin/env bash
        |set -e
        |pkill TempSensor
        |pkill TempControl
        |pkill Fan
        |killall -9 TempSensor_i_App TempControl_i_App Fan_i_App Fan_i_AEP TempControl_i_AEP 2> /dev/null
        |ipcs""".stripMargin
    )

    write(out / 'bin / "compile-mac.sh", """#!/usr/bin/env bash
                                           |set -e
                                           |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                           |cd $SCRIPT_HOME
                                           |rm -fR mac
                                           |mkdir -p mac
                                           |cd $SCRIPT_HOME/../src/c
                                           |mkdir -p mac
                                           |cd mac
                                           |cmake -DCMAKE_BUILD_TYPE=Release ..
                                           |make $MAKE_ARGS
                                           |mv *_App $SCRIPT_HOME/mac/
                                           |mv *_AEP $SCRIPT_HOME/mac/
                                           |mv Main $SCRIPT_HOME/mac/""".stripMargin)

    write(out / 'bin / "compile-linux.sh", """#!/usr/bin/env bash
                                             |set -e
                                             |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                             |cd $SCRIPT_HOME
                                             |rm -fR linux
                                             |mkdir -p linux
                                             |cd $SCRIPT_HOME/../src/c
                                             |mkdir -p linux
                                             |cd linux
                                             |cmake -DCMAKE_BUILD_TYPE=Release ..
                                             |make $MAKE_ARGS
                                             |mv *_App $SCRIPT_HOME/linux/
                                             |mv *_AEP $SCRIPT_HOME/linux/
                                             |mv Main $SCRIPT_HOME/linux/""".stripMargin)

    for (f <- (out / 'bin).toIO.listFiles((_, name) => name.endsWith(".sh"))) {
      %('chmod, "+x", 'bin / f.getName)(out)
    }

    val currentFile = Path(new File(implicitly[sourcecode.File].value).getParentFile) / up / 'c / "ext.c"
    Cli(File.pathSeparatorChar).parseSireum(
      ISZ(
        "transpiler",
        "c",
        "--verbose",
        "--sourcepath",
        s"${out / 'src / 'scala}",
        "--string-size",
        "256",
        "--sequence-size",
        "16",
        "--sequence",
        "ISZ[org.sireumString]=2",
        "--apps",
        "building_control_gen_periodic.Fan_i_AEP,building_control_gen_periodic.Fan_i_App,building_control_gen_periodic.TempControl_i_AEP,building_control_gen_periodic.TempControl_i_App,building_control_gen_periodic.TempSensor_i_App,building_control_gen_periodic.Main",
        "--forward",
        "art.ArtNative=building_control_gen_periodic.ArtNix,building_control_gen_periodic.Platform=building_control_gen_periodic.PlatformNix",
        "--exts",
        currentFile.toString,
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
      case Some(_: Cli.HelpOption) => 0
      case _ => -1
    }
  }

}
