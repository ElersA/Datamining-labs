
import scala.collection.mutable
import scala.io.Source

object PaperImplementation {

  val random = scala.util.Random
  random.setSeed(0L)
  var counters: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  var sample: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
  var T: Double = 0 // Counter for number of global triangles
  var t = 0 // Edge counter

  var M_threshold = 6
  val isTest = false

  def readData(isTest: Boolean): Iterator[String] = {
    if (isTest) {
      Source.fromFile("./src/main/scala/data/p2p-Gnutella06.txt").getLines
    } else {
      Source.fromFile("./src/main/scala/data/validationGraph").getLines
      //Source.fromFile("./src/main/scala/data/actor-collaboration/out.actor-collaboration").getLines
    }
  }

  def main(args: Array[String]): Unit = {
    val dataStream = readData(isTest)

    dataStream
      .filterNot(line => line.startsWith("#") || line.startsWith("%")) // the test dataset contains some comments in the beginning, we don't want those
      .map{twoNodes =>
      val nodes = if (isTest) twoNodes.split("\t") else twoNodes.split(" ")
      (nodes(0).toInt, nodes(1).toInt)
      }
      .foreach { nodes =>
        t += 1
        updateCounters(nodes)
        if (reservoirSampling(nodes)) {
          sample.+=((nodes._1, nodes._2))
        }
      }
    //println(s"Number of counters: ${counters.size}")
    printResults()
  }

  def reservoirSampling(nodes: (Int, Int)): Boolean = {
    if (t <= M_threshold) {
      true
    } else if (random.nextDouble() <= M_threshold.toDouble / t) {
      val sampleAsVector = sample.toVector
      val elementToRemove = sampleAsVector(random.nextInt(sampleAsVector.length)) // Get a random element
      sample.remove(elementToRemove)
      counters.remove(elementToRemove._1)
      true
    } else {
      false
    }
  }

  def updateCounters(nodes: (Int, Int)): Unit = {

    val nodeOnesNeighbors = sample.collect {
      case (a, b) if a == nodes._1 => b
      case (a, b) if b == nodes._1 => a
    }.toSet
    val nodeTwosNeighbors = sample.collect {
      case (a, b) if a == nodes._2 => b
      case (a, b) if b == nodes._2 => a
    }.toSet

    val commonNeighbourhood = nodeOnesNeighbors.intersect(nodeTwosNeighbors)

    commonNeighbourhood.foreach { commonNode =>
      println("Found triangle")
      val increment = math.max(1, ((t - 1) * (t - 2)).toDouble / (M_threshold * (M_threshold - 1)))
      T = T + increment

      /*
      val commonNodeCounter = counters.getOrElse(commonNode, 0)
      val nodeOneCounter = counters.getOrElse(nodes._1, 0)
      val nodeTwoCounter = counters.getOrElse(nodes._2, 0)

      counters.update(commonNode, commonNodeCounter + increment)
      counters.update(nodes._1, nodeOneCounter + increment)
      counters.update(nodes._2, nodeTwoCounter + increment)
      */
      /*
      if (commonNodeCounter == 1 && mode == -1) {
        counters.remove(commonNode)
      } else if (nodeOneCounter == 1 && mode == -1) {
        counters.remove(nodes._1)
      } else if (nodeOneCounter == 1 && mode == -1) {
        counters.remove(nodes._2)
      }
      */
    }
  }

  def printResults(): Unit = {
    println(s"Number of edges: $t")
    println(s"Global triangles: ${math.round(T)}")
    println(s"Global triangles: $T")
    /*
    counters.foreach{case (key, counter) =>
      println(s"$key has $counter triangles")
    }*/
  }
}
