
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

  val documents: List[List[Char]] = readDocuments("./src/main/scala/documents")
  val similarity = 0.8
  var count =0

  //foreach document get the sortedset containing the hashedshingles
  val shingledDocs = documents.map(doc => SimilarItems.shingling(9,doc))

  //compare the documents and print the documents with a jacardsimilarity over 0.8
  val t0 = System.currentTimeMillis()
  var i = 0
  var j = 0
  while (i<shingledDocs.length){
    while (j<shingledDocs.length){
      if(j!=i){
        println("Document" + i +" and Document "+ j + " are similar")
        println(SimilarItems.compareSets(shingledDocs(i),shingledDocs(j)))
      }
      j+=1
    }
    i+=1
    j=i
  }
  val t1 = System.currentTimeMillis()
  println("Elapsed time: " + (t1 - t0) + "ms")

  //List of doucment signatures
  //convert the hashed shingles to signatures

  val minHashed = shingledDocs.map(shingles => SimilarItems.minHashing(100,shingles))
  i = 0
  j = 0
  while (i<minHashed.length){
    while (j<minHashed.length){
      if(j!=i){

        println(SimilarItems.compareSignatures(minHashed(i),minHashed(j)))
      }
      j+=1
    }
    i+=1
    j=i
  }

  val candidatePairs = SimilarItems.LSH(minHashed,0.3)

  println(candidatePairs)

  candidatePairs.foreach{x =>
    println(SimilarItems.compareSignatures(minHashed(x._1),minHashed(x._2)))
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
