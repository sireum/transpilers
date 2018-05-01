package org.sireum.transpiler.c.util

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.sireum._
import org.sireum.U32._

object Fingerprint_Ext {

  def string(s: String, width: Z): String = {
    bytes(s, width).map("%02x" format _).mkString
  }

  def bytes(s: String, width: Z): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(s.value.getBytes(StandardCharsets.UTF_8))
    val max = if (0 < width && width <= 64) width.toInt else 64
    val r = new Array[Byte](max)
    System.arraycopy(hash, 0, r, 0, max)
    r
  }
}
