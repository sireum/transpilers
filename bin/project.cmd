::/*#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF           #
if [ -z "${SIREUM_HOME}" ]; then                      #
  echo "Please set SIREUM_HOME env var"               #
  exit -1                                             #
fi                                                    #
exec "${SIREUM_HOME}/bin/sireum" slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
"%SIREUM_HOME%\bin\sireum.bat" slang run "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.Project

val library = "library"

val alir = "alir"

val frontend = "slang-frontend"

val transpilers = "transpilers"
val common = "common"
val c = "c"

val homeDir = Os.slashDir.up.canon

val commonShared = moduleSharedPub(
  id = s"$transpilers-$common",
  baseDir = homeDir / common,
  sharedDeps = ISZ(alir),
  sharedIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "Slang Transpilers Common Library",
    url = "github.com/sireum/transpilers",
    licenses = bsd2,
    devs = ISZ(robby)
  )
)

val (cShared, cJvm) = moduleSharedJvmPub(
  baseId = s"$transpilers-$c",
  baseDir = homeDir / c,
  sharedDeps = ISZ(commonShared.id),
  sharedIvyDeps = ISZ(),
  jvmDeps = ISZ(library, frontend),
  jvmIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "Slang-to-C Transpiler",
    url = "github.com/sireum/transpilers",
    licenses = bsd2,
    devs = ISZ(robby)
  )
)

val project = Project.empty + commonShared + cShared + cJvm

projectCli(Os.cliArgs, project)