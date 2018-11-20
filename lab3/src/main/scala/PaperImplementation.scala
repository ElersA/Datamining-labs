import org.apache.spark.SparkConf
import org.apache.spark.SparkContext._
import org.apache.spark.streaming.Seconds
import org.apache.spark.streaming.StreamingContext
import org.apache.spark.streaming.StreamingContext._

import scala.collection.mutable
import scala.io.Source

object PaperImplementation extends App{

  var M_threshold = 0;
  val random = scala.util.Random
  random.setSeed(0L)
  var sample: mutable.Map[Int, (Int, Set[Int])] = mutable.Map[Int, (Int, Set[Int])]()
  var T = 0 // Counter for number of global triangles
  // read data

  something.foreach{something =>
    // increment counter
    if sample.reservoirSampling(something, counter) {
      sample.addadd edge to sample
      sample.updateCounters
    }
  }

  def reservoirSampling(nodes: (Int, Int), t: Int): Boolean = {
    if (t <= M_threshold) {
      true
    } else if (random.nextDouble() <= M_threshold.toDouble / t) {
      val toRemove = sample.toVector(random.nextInt(M_threshold))
      sample.remove(toRemove)
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
}
