import scala.io.Source

object Tester extends App {

  val isTest = true
  val M_threshold = 6000

  println(s"Parameters:\nisTest = $isTest\nM = $M_threshold\n")
  println("<TriestBase results>")
  time("Execution time: "){
    TriestBase.start(readData(isTest), M_threshold)
  }

  println("\n<TriestImproved results>")
  time("Execution time: "){
    TriestImproved.start(readData(isTest), M_threshold)
  }

  // From http://biercoff.com/easily-measuring-code-execution-time-in-scala/ (slightly modified)
  def time[R](message: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(message + (t1 - t0) + "ms")
    result
  }

  def readData(isTest: Boolean): Iterator[(Int, Int)] = {
    if (isTest) {
      /*
      Source
        .fromFile("./src/main/scala/data/p2p-Gnutella06.txt")
        .getLines
        .filterNot(line => line.startsWith("#"))
        .map{twoNodes =>
          val nodes = twoNodes.split("\t")
          (nodes(0).toInt, nodes(1).toInt)
        }
      */

      Source
        .fromFile("./src/main/scala/data/validationGraph")
        .getLines
        .map{twoNodes =>
          val nodes = twoNodes.split(" ")
          (nodes(0).toInt, nodes(1).toInt)
        }

    } else {
      Source
        .fromFile("./src/main/scala/data/actor-collaboration/out.actor-collaboration")
        .getLines
        .filterNot(line => line.startsWith("#")) // the test dataset contain some comments in the beginning, we don't want those
        .map{twoNodes =>
        val nodes = twoNodes.split(" ")
        (nodes(0).toInt, nodes(1).toInt)
      }
    }
  }
}
