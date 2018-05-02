package org.sireum.transpiler.c

import org.sireum._

object Runtime_Ext {

  def staticFiles: HashMap[ISZ[String], String] = {
    val map = org.sireum.$internal.RC.text(Seq("runtime/shared/src", "runtime/static/src")) {
      (_, _) => true
    }
    var r = HashMap.empty[ISZ[String], String]
    for ((k, v) <- map) {
      val key = ISZ(k.map(s => String(s)): _*)
      val value = String(v)
      r = r + key ~> value
    }
    r
  }
}
