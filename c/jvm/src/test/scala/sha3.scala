// #Sireum
import org.sireum._

object sha3 extends App {
  def main(args: ISZ[String]): Z = {
    if (args.size == 0 || args.size > 2) {
      printUsage()
    } else if (args(0) == "512") {
      if (args.size == 1) {
        processStdIn(crypto.SHA3.init512)
      } else {
        val hash = crypto.SHA3.sum512(conversions.String.toU8is(args(1)))
        printHash(hash)
      }
    } else if (args(0) == "384") {
      if (args.size == 1) {
        processStdIn(crypto.SHA3.init384)
      } else {
        val hash = crypto.SHA3.sum384(conversions.String.toU8is(args(1)))
        printHash(hash)
      }
    } else if (args(0) == "256") {
      if (args.size == 1) {
        processStdIn(crypto.SHA3.init256)
      } else {
        val hash = crypto.SHA3.sum256(conversions.String.toU8is(args(1)))
        printHash(hash)
      }
    } else {
      printUsage()
    }
    return 0
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
    println("Usage: sha3 <( 256 | 384 | 512 )> [ <input> ]")
  }
}

@ext object Console {
  def read(): ISZ[U8] = $
}