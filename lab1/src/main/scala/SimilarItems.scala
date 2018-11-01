import scala.collection.SortedSet
import scala.io.Source
import scala.util.Random

object SimilarItems {

  // For testing
  val primes: List[Long] = Source.fromFile("./primes").getLines.toList.map(_.toLong) // From https://www.bigprimes.net/archive/prime/13999999/
  def readFile: List[Char] = Source.fromFile("./tempData.txt").getLines.toList.flatten

  def shingling(k: Int, document: List[Char]): SortedSet[Long] = {
    val kShingles: Set[List[Char]] = document.sliding(k).toSet
    val hashedShingles = kShingles.map(_.hashCode()) // TODO maybe use abs to avoid negative numbers
    SortedSet[Int]() ++ hashedShingles // Convert our Set to a SortedSet
  }

  def compareSets(a: SortedSet[Long], b: SortedSet[Long]): Double = {
    val numerator = a.intersect(b).size
    val denominator = a.union(b).size

    numerator / denominator match {
      case 0 => throw new IllegalArgumentException("Do not divide by zero")
      case _ => numerator / denominator
    }
  }

  def minHashing(n: Int, hashedShingles: SortedSet[Long]): Set[Long] = {

    primes map {prime =>
      var tempMin = hashFunc(prime)(hashedShingles.head) // Initial value
      val hashFuncWithPrime = hashFunc(prime)

      hashedShingles.fold(tempMin){ (acc, elem) =>
        if (hashFuncWithPrime(elem) < acc) elem else acc
      }

    }

    // Maybe use for expression!

    for {
      prime <- primes
      min <- getMin
    } yield min


    def hashFunc(prime: Long)(hashedShingle: Long): Long = {
      /*
        hash function: (ax+b) % c
          a and b are random values
          c is a large prime.
          x is an element from the input set
      */
      val rnd = new Random
      rnd.setSeed(0)
      val a = rnd.nextLong * hashedShingle
      val b = rnd.nextLong * hashedShingle
      (a * hashedShingle) % prime
    }
  }

  def main(args: Array[String]): Unit = {

  }

}
