
import SimilarItems._
import scala.io.Source
import java.io.File

object Tester extends App {

  /*
      Test program
      Test scalability
   */

  // TODO remake
  val documents: List[List[Char]] = readDocuments("./documents")
  val similarity = 0.8


  /* Code to measure time
    http://biercoff.com/easily-measuring-code-execution-time-in-scala/

    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
   */

  // TODO check if this works
  def readDocuments(directoryPath: String): List[List[Char]] = {
    val files = new File(directoryPath).listFiles()
    files.map(file => Source.fromFile(file).getLines.toList.flatten).toList
  }

}
