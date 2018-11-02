
import SimilarItems._

import scala.io.Source
import java.io.File

import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer

object Tester extends App {

  /*
      Test program
      Test scalability
   */

  // TODO remake
  val documents: List[List[Char]] = readDocuments("./documents")
  val similarity = 0.8
  //List of documents hashedshingles
  val listOfShinglings = new ListBuffer[SortedSet[Int]]()
  var count =0
  //foreach document get the sortedset containing the hashedshingles
  documents.foreach{doc =>
    listOfShinglings.append(SimilarItems.shingling(9,doc))
  }

  //compare the documents and print the docus with a jacardsimilarity over 0.8

  SimilarItems.compareSets()

  //List of doucment signatures
  val listOfSignatures = new ListBuffer[Set[Int]]()
  //convert the hashed shingles to signatures
  listOfShinglings.foreach{shing =>
    listOfSignatures.append(SimilarItems.minHashing(100,shing))
  }

  //compare the signatures of the doucments
  //print doucment that are similar >0.8

  //apply lsh on the signatures

  //take the list of suggested pairs and compare their signatures.
  //print document that are similar >0.8



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
