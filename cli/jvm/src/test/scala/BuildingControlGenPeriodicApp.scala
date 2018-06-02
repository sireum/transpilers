import org.sireum._
import org.sireum.transpiler.{CTranspiler, Cli}
import java.io.File
import ammonite.ops._

object BuildingControlGenPeriodicApp extends scala.App {

  if (args.length != 1) {
    println("Usage: BuildingControlGenPeriodicApp <slang-embedded-path>")
  } else {

    val out = root / 'Volumes / 'RAM / 'out
    //val out = pwd / 'output
    val path = args(0)
    val currentFile = Path(new File(implicitly[sourcecode.File].value).getParentFile) / up / 'c / "ext.c"
    Cli(File.pathSeparatorChar).parseSireum(
      ISZ(
        "transpiler",
        "c",
        "--verbose",
        "--sourcepath",
        s"$path/art:$path/building-control-gen-periodic/src/main",
        "--apps",
        "building_control_gen_periodic.Fan_i_AEP,building_control_gen_periodic.Fan_i_App,building_control_gen_periodic.TempControl_i_AEP,building_control_gen_periodic.TempControl_i_App,building_control_gen_periodic.TempSensor_i_App",
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
    rm ! out / 'CMakeFiles
    rm ! out / "cmake_install.cmake"
    rm ! out / "CMakeCache.txt"
    %('make, "-j8")(out)
  }

}
