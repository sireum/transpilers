package org.sireum.transpiler.c.util

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.sireum._
import org.sireum.U32._

object Fingerprint_Ext {

  def string(s: String): String = {
    bytes(s).map("%02x" format _).mkString
  }

  def u32(s: String): U32 = {
    val bs = bytes(s)
    var r = u32"0"
    for (i <- bs.indices) {
      r = r | (U32(bs(i) & 0xFF) << U32(i * 8))
    }
    r
  }

  def bytes(s: String): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(s.value.getBytes(StandardCharsets.UTF_8))
    val max = 3
    val r = new Array[Byte](max)
    System.arraycopy(hash, 0, r, 0, max)
    r
  }
}
