// #Sireum
import org.sireum._

object SHA3 extends App {
  def main(args: ISZ[String]): Z = {
    if (args.size > 1) {
      printUsage()
    } else if (args.size == 0 || args(0) == "512") {
      processInput(crypto.SHA3.init512)
    } else if (args(0) == "384") {
      processInput(crypto.SHA3.init384)
    } else if (args(0) == "256") {
      processInput(crypto.SHA3.init256)
    } else {
      printUsage()
    }
    return 0
  }

  def processInput(sha3: crypto.SHA3): Unit = {
    var in = Console.read()
    while (in.size > 0) {
      sha3.update(in)
      in = Console.read()
    }
    val hash = sha3.finalise()
    for (i <- z"0" until hash.size) {
      print(hash(i))
    }
    println()
  }

  def printUsage(): Unit = {
    println("Usage: sha3 [ 256 | 384 | 512 ]")
    println("            (default: 512)")
  }
}

@ext object Console {
  def read(): ISZ[U8] = $
}