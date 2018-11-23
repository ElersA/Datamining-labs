
import scala.collection.mutable

object TriestImproved {

  val random = scala.util.Random
  random.setSeed(0L)
  var counters: mutable.Map[Int, Double] = mutable.Map[Int, Double]()
  var sample: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
  var T: Double = 0 // Counter for number of global triangles
  var t = 0 // Edge counter
  var M_threshold = 0

  def start(dataStream: Iterator[(Int, Int)], M_threshold: Int, window_Size :Int): Unit = {
    this.M_threshold = M_threshold

    dataStream
      .foreach { nodes =>
        if (window_Size > t) {
          t += 1
          updateCounters(nodes)
          if (reservoirSampling(nodes)) {
            sample.+=((nodes._1, nodes._2))
          }
        }
        if (window_Size == t) {
          //if window size reached print results and reset counters and the sample
          println("--Window results--")
          printResults()
          t = 0
          T = 0
          counters = mutable.Map[Int, Double]()
          sample = mutable.Set[(Int, Int)]()
        }
      }
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
      val increment = math.max(1, ((t - 1) * (t - 2)).toDouble / (M_threshold * (M_threshold - 1)))
      T = T + increment

      val commonNodeCounter: Double = counters.getOrElse(commonNode, 0)
      val nodeOneCounter: Double = counters.getOrElse(nodes._1, 0)
      val nodeTwoCounter: Double = counters.getOrElse(nodes._2, 0)

      counters.update(commonNode, commonNodeCounter + increment)
      counters.update(nodes._1, nodeOneCounter + increment)
      counters.update(nodes._2, nodeTwoCounter + increment)
    }
  }

  def printResults(): Unit = {
    println(s"Number of edges: $t")
    println(s"Global triangles: ${math.round(T)}")
    //println(s"Global triangles: $T")
    /*
    counters.foreach{case (key, counter) =>
      println(s"$key has $counter triangles")
    }*/
  }
}
