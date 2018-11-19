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




}
