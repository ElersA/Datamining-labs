import scala.collection.SortedSet
import scala.io.Source
import scala.util.Random

object SimilarItems {

  // For testing
  val primes: List[Long] = Source.fromFile("./primes").getLines.toList.map(_.toLong) // From https://www.bigprimes.net/archive/prime/13999999/
  def readFile: List[Char] = Source.fromFile("./tempData.txt").getLines.toList.flatten

  def shingling(k: Int, document: List[Char]): SortedSet[Int] = {
    val kShingles: Set[List[Char]] = document.sliding(k).toSet
    val hashedShingles = kShingles.map(_.hashCode()) // TODO maybe use abs to avoid negative numbers
    SortedSet[Int]() ++ hashedShingles // Convert our Set to a SortedSet
  }

  def compareSets(a: SortedSet[Int], b: SortedSet[Int]): Double = {
    val numerator = a.union(b).size
    val denominator = a.intersect(b).size

    numerator / denominator match {
      case 0 => throw new IllegalArgumentException("Do not divide by zero")
      case _ => numerator / denominator
    }
  }

  def minHashing(n: Int, hashedShingles: SortedSet[Int]): Set[Int] = {

    val rnd = new Random
    rnd.setSeed(0)

    foreach hashfunc selectMin -> vector

    // Hash everything
    primes map {prime =>
      hashedShingles.map {shingleHash =>
        val a = rnd.nextInt(shingleHash)
        val b = rnd.nextInt(shingleHash)

        /* hash function: (ax+b) % c
            a and b are random values and c is a large prime. x is an element from the input set */
        (a*shingleHash) % prime
      }
    }






    primes.map{
      val a = rnd.nextInt()
    }

    /*
        Gå igenom flera hash-funktioner - Ta minsta värdet



     */

  }

  def main(args: Array[String]): Unit = {

  }

}
