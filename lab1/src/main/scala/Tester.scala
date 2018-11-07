
import SimilarItems._
import scala.io.Source
import java.io.File
import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer

object Tester extends App {

  val namesAndDocs: List[(String, List[Char])] = readDocuments("./src/main/scala/documents")
  val (fileNames: List[String], documents: List[List[Char]]) = namesAndDocs.unzip
  val similarity = 0.6
  println(s"Using a similarity threshold of $similarity")

  // foreach document get the SortedSet containing the hashed shingles
  val shingledDocs = documents.map(doc => SimilarItems.shingling(9,doc)).zip(fileNames)

  // compare the documents and print the documents with a Jaccard similarity over 0.8
  val t0 = System.currentTimeMillis()
  var i = 0
  var j = 0
  while (i<shingledDocs.length){
    while (j<shingledDocs.length){
      if(j!=i){
        val jaccardSimilarity = SimilarItems.compareSets(shingledDocs(i)._1,shingledDocs(j)._1)
        if (jaccardSimilarity >= similarity) {
          println(s"documents: ${shingledDocs(i)._2} and ${shingledDocs(j)._2} are $jaccardSimilarity equal (JACCARD SIMILARITY)")
        }
      }
      j+=1
    }
    i+=1
    j=i
  }
  val t1 = System.currentTimeMillis()
  println("Elapsed time: " + (t1 - t0) + "ms")

  //List of document signatures. Convert the hashed shingles to signatures
  val minHashed = shingledDocs.map(shingles => SimilarItems.minHashing(100,shingles._1)).zip(fileNames)
  i = 0
  j = 0
  while (i<minHashed.length){
    while (j<minHashed.length){
      if(j!=i){
        val signatureSimilarity = SimilarItems.compareSignatures(minHashed(i)._1, minHashed(j)._1)
        if (signatureSimilarity >= similarity) {
          println(s"documents: ${minHashed(i)._2} and ${minHashed(j)._2} are $signatureSimilarity equal (SIGNATURE SIMILARITY)")
        }
      }
      j+=1
    }
    i+=1
    j=i
  }

  val candidatePairs = SimilarItems.LSH(minHashed.unzip._1, similarity)
  candidatePairs.foreach{pair => println(s"Candidate pair: (${fileNames(pair._1)}, ${fileNames(pair._2)})")}

  val minHashComparisons = ((shingledDocs.size*shingledDocs.size) - shingledDocs.size) / 2 // (n² - n) / 2
  println(s"Number of minHash comparisons = $minHashComparisons")
  println(s"Number of candidate pairs: ${candidatePairs.size}")
  println(s"Number of comparisons reduced by LSH = ${minHashComparisons - candidatePairs.size} (${(minHashComparisons-candidatePairs.size).toDouble / minHashComparisons.toDouble}%)")

  /*
      Code to measure time
      Source: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
   */
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def readDocuments(directoryPath: String): List[(String, List[Char])] = {
    val files = new File(directoryPath).listFiles()
    files.map(file => (file.getName, Source.fromFile(file).getLines.toList.flatten)).toList
  }
}
