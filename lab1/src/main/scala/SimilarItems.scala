import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

object SimilarItems {

  // For testing
  val primes: List[Int] = Source.fromFile("./primes").getLines.toList.map(_.toInt) // From https://www.bigprimes.net/archive/prime/13999999/
  def readFile: List[Char] = Source.fromFile("./tempData.txt").getLines.toList.flatten

  def shingling(k: Int, document: List[Char]): SortedSet[Int] = {
    val kShingles: Set[List[Char]] = document.sliding(k).toSet
    val hashedShingles = kShingles.map(shingle => Math.abs(shingle.hashCode()))
    SortedSet[Int]() ++ hashedShingles // Convert our Set to a SortedSet
  }

  def compareSets(a: SortedSet[Int], b: SortedSet[Int]): Double = {
    val numerator = a.intersect(b).size
    val denominator = a.union(b).size

    numerator / denominator match {
      case 0 => throw new IllegalArgumentException("Do not divide by zero")
      case _ => numerator.toDouble / denominator.toDouble
    }
  }

  def minHashing(n: Int, hashedShingles: SortedSet[Int]): Set[Int] = {
    val rnd = new Random
    rnd.setSeed(0)
    var minval = Int.MaxValue
    val listOfMin = new ListBuffer[Int]()
    // foreach hash apply to every shingle get min value for that hash and save to list
    primes.take(n).foreach { prime =>
      /*
        hash function: (ax+b) % c
          a and b are random values
          c is a large prime.
          x is an element from the input set
      */
      val a = rnd.nextInt(10)
      val b = rnd.nextInt(10)
      hashedShingles.foreach { shingle =>
        var tempmin = (a * shingle + b) % prime
        if (minval < tempmin) {
          minval = tempmin
        }
      }
      listOfMin.append(minval)
      minval = Int.MaxValue
    }
    listOfMin.toSet
  }

  def compareSignatures(a: Set[Int], b: Set[Int]): Double = {
    // Assumption: a & b are of equal size
    val count = a.zip(b).foldLeft(0)((acc: Int, hashTuple: (Int, Int)) => if (hashTuple._1 == hashTuple._2) acc + 1 else acc)
    count.toDouble / a.size.toDouble
  }

  def LSH(minHashes: List[Set[Int]], similarity: Double): List[(Int, Int)] = {
    val b = 20 // Bands. Hardcoded values from the slides
    val r = 5 // Rows. Hardcoded values from the slides

    val buckets = 10000
    val candidatePairHolder: Map[Int, Map[Int, List[Int]]] = Map.empty

    minHashes foreach {hashes =>
      hashes.sliding(r, r).sum

    }

    /*
        1. Iterate over each Set in minHashes
        2. For each Set: create a sliding window of size r and step size r
        3. In each sliding window: sum all values and hash the sum
        4. Save the hash
     */

  }

  def main(args: Array[String]): Unit = {

  }

}
