package org.sireum.transpiler.c.util

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.sireum._

object Fingerprint_Ext {

  def string(s: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(s.value.getBytes(StandardCharsets.UTF_8))
    val max = 3
    val f = new Array[Byte](max)
    System.arraycopy(hash, 0, f, 0, max)
    f.map("%02x" format _).mkString
  }
}
