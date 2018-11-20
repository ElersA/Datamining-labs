
import scala.collection.mutable
import scala.io.Source

object PaperImplementation extends App{

  var M_threshold = 0
  val random = scala.util.Random
  random.setSeed(0L)
  var sample: mutable.Map[Int, (Int, Set[Int])] = mutable.Map[Int, (Int, Set[Int])]()
  var T = 0 // Counter for number of global triangles
  var iteration = 0
  val dataStream: Iterator[String] = readData("./src/main/scala/data/REPLACE THIS") // TODO replace with a file

  dataStream
    .map{twoNodes =>
      val nodes = twoNodes.split(" ")
      (nodes(0).toInt, nodes(1).toInt)
    }
    .foreach{nodes =>
    iteration += 1
    if (reservoirSampling(nodes, iteration)) {
      sample.put(nodes._1, (0, Set(nodes._2)))
      updateCounters(1, nodes)
    }
  }

  def reservoirSampling(nodes: (Int, Int), t: Int): Boolean = {
    if (t <= M_threshold) {
      true
    } else if (random.nextDouble() <= M_threshold.toDouble / t) {
      val keys = sample.keySet.toList
      val randomKey = random.nextInt(keys.size)
      val keyToRemove = keys(randomKey)
      sample.remove(keyToRemove)
      updateCounters(-1, (1,2))
      true
    }
    false
  }

  // Mode can be either 1 for addition or -1 for subtraction
  def updateCounters(mode: Int, nodes: (Int, Int)): Unit = {
    val commonNeighbourhood = sample(nodes._1)._2.intersect(sample(nodes._2)._2)
    commonNeighbourhood.foreach{commonNode =>
      T = T + mode
      sample(commonNode) = (sample(commonNode)._1 + mode, sample(commonNode)._2) // TODO check if we need options
      sample(nodes._1) = (sample(nodes._1)._1 + mode, sample(nodes._1)._2)
      sample(nodes._2) = (sample(nodes._2)._1 + mode, sample(nodes._2)._2)
    }
  }

  def readData(path: String): Iterator[String] = Source.fromFile(path).getLines
}
