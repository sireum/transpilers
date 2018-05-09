import mill._
import mill.scalalib._
import org.sireum.mill.SireumModule._

trait Module extends CrossJvmJs {

  final override def jvmDeps = Seq()

  final override def jsDeps = Seq()

  final override def scalacPluginIvyDeps = Agg(ivy"org.sireum::scalac-plugin:$scalacPluginVersion")

  final override def testIvyDeps = Agg(ivy"org.scalatest::scalatest::$scalaTestVersion")

  final override def jvmTestIvyDeps = Agg(ivy"com.sksamuel.diff:diff:$diffVersion",
    ivy"com.lihaoyi::ammonite-ops:$ammoniteOpsVersion")

  final override def jsTestIvyDeps = Agg.empty

  final override def testScalacPluginIvyDeps = scalacPluginIvyDeps

  final override def jvmTestFrameworks = Seq("org.scalatest.tools.Framework")

  final override def jsTestFrameworks = jvmTestFrameworks
}

object Module {

  trait Common extends Module {

    final override def deps = Seq(alirObject)

    def alirObject: CrossJvmJs

    final override def ivyDeps = Agg.empty
  }

  trait C extends Module {

    final override def deps = Seq(commonObject)

    final override def ivyDeps = Agg.empty

    def commonObject: Common
  }

  trait Cli extends JvmOnly {

    final override def deps = Seq()

    final override def testFrameworks = Seq()

    final override def testIvyDeps = Agg.empty

    final override def testScalacPluginIvyDeps = Agg.empty

    final override def crossDeps = Seq(cObject)

    final override def ivyDeps = Agg(ivy"com.lihaoyi::ammonite-ops:$ammoniteOpsVersion")

    final override def scalacPluginIvyDeps = Agg(ivy"org.sireum::scalac-plugin:$scalacPluginVersion")

    final override def mainClass = Some("org.sireum.transpiler.Sireum")

    def cObject: C
  }

}
