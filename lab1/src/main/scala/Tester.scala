import scala.io.Source
import java.io.File

object Tester extends App {

  val namesAndDocs: List[(String, List[Char])] = readDocuments("./src/main/scala/documentsBigLarge")
  val (fileNames: List[String], documents: List[List[Char]]) = namesAndDocs.unzip
  val similarity = 0.8
  println(s"Using a similarity threshold of $similarity")

  // Map each document to a SortedSet containing the hashed shingles
  val shingledDocs = documents.map(doc => SimilarItems.shingling(9, doc)).zip(fileNames)

  // Compare the documents and print the documents with a Jaccard similarity over 0.8
  time ("Jaccard similarity"){
    shingledDocs.indices.foreach{ i =>
      shingledDocs.indices.foreach{ j =>
        if (j != i) {
          val jaccardSimilarity = SimilarItems.compareSets(shingledDocs(i)._1, shingledDocs(j)._1)
          if (jaccardSimilarity >= similarity) {
            println(s"documents: ${shingledDocs(i)._2} and ${shingledDocs(j)._2} are $jaccardSimilarity equal (JACCARD SIMILARITY)")
          }
        }
      }
    }
  }

  // List of document signatures. Convert the hashed shingles to signatures
  val minHashed = shingledDocs.map(shingles => SimilarItems.minHashing(100, shingles._1)).zip(fileNames)
  time ("Signature comparison (without LSH)"){
    minHashed.indices.foreach{ i =>
      minHashed.indices.foreach{ j =>
        if (j != i) {
          val signatureSimilarity = SimilarItems.compareSignatures(minHashed(i)._1, minHashed(j)._1)
          if (signatureSimilarity >= similarity) {
            println(s"documents: ${minHashed(i)._2} and ${minHashed(j)._2} are $signatureSimilarity equal (SIGNATURE SIMILARITY)")
          }
        }
      }
    }
  }

  val candidatePairs = SimilarItems.LSH(minHashed.unzip._1, similarity)
  time("Signature comparison time (with LSH)"){
    candidatePairs.foreach { pair =>
      println(s"Candidate pair: (${fileNames(pair._1)}, ${fileNames(pair._2)})")
      candidatePairs.foreach { pair => SimilarItems.compareSignatures(minHashed(pair._1)._1, minHashed(pair._2)._1) }
    }
  }

  val minHashComparisons = ((shingledDocs.size * shingledDocs.size) - shingledDocs.size) / 2 // (n² - n) / 2
  println(s"Number of minHash comparisons = $minHashComparisons")
  println(s"Number of candidate pairs: ${candidatePairs.size}")
  println(s"Number of comparisons reduced by LSH = ${minHashComparisons - candidatePairs.size} (${(minHashComparisons - candidatePairs.size).toDouble / minHashComparisons.toDouble}%)")

  // Source: http://biercoff.com/easily-measuring-code-execution-time-in-scala/. A bit modified though.
  def time[R](messageString: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(messageString + " :: Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def readDocuments(directoryPath: String): List[(String, List[Char])] = {
    val files = new File(directoryPath).listFiles()
    files.map(file => (file.getName, Source.fromFile(file)(io.Codec.ISO8859).getLines.toList.flatten)).toList
  }
}
