import scala.io.Source

object Tester extends App {

  val isTest = true
  val M_threshold = 88234
  val window_size = readData(isTest).length

  println(s"Parameters:\nisTest = $isTest\nM = $M_threshold\nWindow size: $window_size\n")
  println("<TriestBase results>")
  time("Execution time: "){
    TriestBase.start(readData(isTest), M_threshold, window_size)
  }

  println("\n<TriestImproved results>")
  time("Execution time: "){
    TriestImproved.start(readData(isTest), M_threshold, window_size)
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
      // Directed dataset. Don't use it. It works though.
      // https://snap.stanford.edu/data/p2p-Gnutella06.html
      // Number of global triangles: 1142
      // Number of edges: 31525
      Source
        .fromFile("./src/main/scala/data/p2p-Gnutella06.txt")
        .getLines
        .filterNot(line => line.startsWith("#"))
        .map{twoNodes =>
          val nodes = twoNodes.split("\t")
          (nodes(0).toInt, nodes(1).toInt)
        }
      */

      // https://snap.stanford.edu/data/ego-Facebook.html
      // Number of global triangles: 1612010
      // Number of edges: 88234
      Source
        .fromFile("./src/main/scala/data/facebook_combined.txt")
        .getLines
        .map{twoNodes =>
          val nodes = twoNodes.split(" ")
          (nodes(0).toInt, nodes(1).toInt)
        }

      /*
      // Number of global triangles: 9
      // Number of edges: 27
      Source
        .fromFile("./src/main/scala/data/validationGraph")
        .getLines
        .map{twoNodes =>
          val nodes = twoNodes.split(" ")
          (nodes(0).toInt, nodes(1).toInt)
        }
      */
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
