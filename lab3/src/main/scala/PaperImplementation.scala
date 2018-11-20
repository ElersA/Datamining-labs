import org.apache.spark.SparkConf
import org.apache.spark.SparkContext._
import org.apache.spark.streaming.Seconds
import org.apache.spark.streaming.StreamingContext
import org.apache.spark.streaming.StreamingContext._
import scala.io.Source

object PaperImplementation extends App{

  // Set up the Spark configuration with the app name and use two threads
  val sparkConf = new SparkConf().setAppName("id2221proj").setMaster("local[2]")

  // Use the config to create a streaming context with a batch interval of every 5 seconds.
  val ssc = new StreamingContext(sparkConf, Seconds(5))

  val M_threshold = 6
  val random = scala.util.Random
  random.setSeed(0L)
  var S: scala.collection.mutable.Set[(Int, Int)] = Set[(Int, Int)]()



  def reservoirSampling(nodeAndEdge: (Int, Int), t: Int): Boolean = {
    if (t <= M_threshold) {
      true
    } else if (random.nextDouble() <= M_threshold.toDouble / t) {
      val toRemove = S.toVector(random.nextInt(M_threshold))
      S.remove(toRemove)
      updateCounters(-1, (1,2))
      true
    }
    false
  }

  // Mode can be either 1 for addition or -1 for subtraction
  def updateCounters(mode: Int, nodeAndEdge: (Int, Int)): Unit = {

  }

}
