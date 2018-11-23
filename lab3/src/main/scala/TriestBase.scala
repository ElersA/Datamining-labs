
import scala.collection.mutable

object TriestBase {

  val random = scala.util.Random
  random.setSeed(0L)
  var counters: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  var sample: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
  var T :Double = 0 // Counter for number of global triangles
  var t = 0 // Edge counter
  var M_threshold = 0

  def start(dataStream: Iterator[(Int, Int)], M_threshold: Int,  window_Size :Int): Unit = {
    this.M_threshold = M_threshold
    
    dataStream
      .foreach { nodes =>
        if (window_Size > t) {
          t += 1
          if (reservoirSampling(nodes, t)) {
            sample.+=((nodes._1, nodes._2))
            updateCounters(1, nodes)
          }
        }

    else {
      //if window size reached print results and reset counters and the sample
      T = T * math.max(1, ((t - 1) * (t - 2)).toDouble / (M_threshold * (M_threshold - 1)))

      printResults()
      t = 0
      T = 0
      counters: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
      sample: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
    }
    //println(s"Number of counters: ${counters.size}")
  }
  }

  def reservoirSampling(nodes: (Int, Int), t: Int): Boolean = {
    if (t <= M_threshold) {
      true
    } else if (random.nextDouble() <= M_threshold.toDouble / t) {
      val sampleAsVector = sample.toVector
      val elementToRemove = sampleAsVector(random.nextInt(sampleAsVector.length)) // Get a random element
      sample.remove(elementToRemove)
      counters.remove(elementToRemove._1)
      updateCounters(-1, elementToRemove) // Decrement counters
      true
    } else {
      false
    }
  }

  // Mode can be either 1 for addition or -1 for subtraction
  def updateCounters(mode: Int, nodes: (Int, Int)): Unit = {

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
      T = T + mode

      val commonNodeCounter = counters.getOrElse(commonNode, 0)
      val nodeOneCounter = counters.getOrElse(nodes._1, 0)
      val nodeTwoCounter = counters.getOrElse(nodes._2, 0)

      counters.update(commonNode, commonNodeCounter + mode)
      counters.update(nodes._1, nodeOneCounter + mode)
      counters.update(nodes._2, nodeTwoCounter + mode)

      if (commonNodeCounter == 1 && mode == -1) {
        counters.remove(commonNode)
      } else if (nodeOneCounter == 1 && mode == -1) {
        counters.remove(nodes._1)
      } else if (nodeOneCounter == 1 && mode == -1) {
        counters.remove(nodes._2)
      }
    }
  }

  def printResults(): Unit = {
    println(s"Number of edges: $t")
    println(s"Global triangles: $T")
    /*
    counters.foreach{case (key, counter) =>
      println(s"$key has $counter triangles")
    }*/
  }
}