import org.sireum.{ISZ, U8}

object Console_Ext {
  def read(): ISZ[U8] = {
    val in = new java.util.Scanner(System.in)
    val buffer = scala.collection.mutable.ArrayBuffer[U8]()
    while (in.hasNextByte) {
      buffer += U8(in.nextByte())
    }
    return ISZ(buffer: _*)
  }
}
