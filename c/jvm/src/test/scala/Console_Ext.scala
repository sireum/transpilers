import org.sireum.{ISZ, U8}

object Console_Ext {
  def read(): ISZ[U8] = {
    val buffer = scala.collection.mutable.ArrayBuffer[U8]()
    var b = System.in.read()
    while (b != -1) {
      buffer += U8(b)
      b = System.in.read()
    }
    return ISZ(buffer: _*)
  }
}
