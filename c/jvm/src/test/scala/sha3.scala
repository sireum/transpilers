// #Sireum
import org.sireum._

object sha3 extends App {
  def main(args: ISZ[String]): Z = {
    if (args.isEmpty || args.size > 2) {
      printUsage()
      return 0
    }
    val arg0 = args(0)
    val size1 = args.size == 1
    var stdIn = F
    val sha3: crypto.SHA3 = arg0 match {
      case string"256" =>
        stdIn = size1
        crypto.SHA3.init256
      case string"384" =>
        stdIn = size1
        crypto.SHA3.init384
      case string"512" =>
        stdIn = size1
        crypto.SHA3.init512
      case _ =>
        crypto.SHA3.init512
    }
    if (stdIn) {
      processStdIn(sha3)
    } else if (size1) {
      processString(sha3, arg0)
    } else {
      processString(sha3, args(1))
    }
    return 0
  }

  def processString(sha3: crypto.SHA3, input: String): Unit = {
    sha3.update(conversions.String.toU8is(input))
    val hash = sha3.finalise()
    printHash(hash)
  }

  def processStdIn(sha3: crypto.SHA3): Unit = {
    var in = Console.read()
    while (in.size > 0) {
      sha3.update(in)
      in = Console.read()
    }
    val hash = sha3.finalise()
    printHash(hash)
  }

  def printHash(hash: ISZ[U8]): Unit = {
    for (i <- z"0" until hash.size) {
      print(hash(i))
    }
    println()
  }

  def printUsage(): Unit = {
    println("Usage: sha3 [ 256 | 384 | 512 ] [ <input> ]")
    println("            (default: 512)")
  }
}

@ext object Console {
  def read(): ISZ[U8] = $
}