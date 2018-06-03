import org.sireum._
import org.sireum.transpiler.{CTranspiler, Cli}
import java.io.File
import ammonite.ops._

object BuildingControlGenPeriodicApp extends scala.App {

  if (args.length != 2) {
    println("Usage: BuildingControlGenPeriodicApp <slang-embedded-path> <output-path>")
  } else {

    val path = Path(new File(args(0)).getCanonicalFile.getAbsoluteFile)
    val out = Path(new File(args(1)).getCanonicalFile.getAbsoluteFile)
    val currentFile = Path(new File(implicitly[sourcecode.File].value).getParentFile) / up / 'c / "ext.c"
    Cli(File.pathSeparatorChar).parseSireum(
      ISZ(
        "transpiler",
        "c",
        "--verbose",
        "--sourcepath",
        s"$path/art:$path/building-control-gen-periodic/src/main",
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
        out.toString
      ),
      0
    ) match {
      case Some(o: Cli.CTranspilerOption) => CTranspiler.run(o)
      case Some(_: Cli.HelpOption) => 0
      case _ => -1
    }

    %('cmake, ".")(out)
    %('make, "-j8")(out)
    rm ! out / 'CMakeFiles
    rm ! out / "cmake_install.cmake"
    rm ! out / "CMakeCache.txt"
    write.over(out / "run-mac.sh", """#!/usr/bin/env bash -e
                                     |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                     |cd $SCRIPT_HOME
                                     |./TempControl_i_AEP 2> /dev/null &
                                     |./Fan_i_AEP 2> /dev/null &
                                     |open -a Terminal ./TempControl_i_App
                                     |open -a Terminal ./TempSensor_i_App
                                     |open -a Terminal ./Fan_i_App
                                     |read -p "Press enter to start ..."
                                     |./Main""".stripMargin)
    write.over(out / "run-nix.sh", """#!/usr/bin/env bash -e
                                     |export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
                                     |cd $SCRIPT_HOME
                                     |./TempControl_i_AEP 2> /dev/null &
                                     |./Fan_i_AEP 2> /dev/null &
                                     |xterm -e sh -c ./TempControl_i_App &
                                     |xterm -e sh -c ./TempSensor_i_App &
                                     |xterm -e sh -c ./Fan_i_App &
                                     |read -p "Press enter to start ..."
                                     |./Main""".stripMargin)
    write.over(out / "stop.sh", """#!/usr/bin/env bash -e
                                  |pkill TempSensor_i_App TempControl_i_App Fan_i_App Fan_i_AEP TempControl_i_AEP
                                  |killall -9 TempSensor_i_App TempControl_i_App Fan_i_App Fan_i_AEP TempControl_i_AEP
                                  |ipcs""".stripMargin)
    %('chmod, "+x", "run-mac.sh")(out)
    %('chmod, "+x", "run-nix.sh")(out)
    %('chmod, "+x", "stop.sh")(out)
  }

}
