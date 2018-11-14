import scala.collection.SortedSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import scala.collection.mutable.Set
import scala.io.Source

object SimilarItems {

  // List of factors to use in minHashing
  val factorList: List[Int] = Source.fromFile("./src/main/scala/factorFile").getLines.toList.map(_.toInt)

  // Create shingles of length k from the list of characters and return a SortedSet containing the shingles' hashes
  def shingling(k: Int, document: List[Char]): SortedSet[Int] = {
    val kShingles: Predef.Set[List[Char]] = document.sliding(k).toSet
    val hashedShingles = kShingles.map(shingle => Math.abs(shingle.hashCode()))
    SortedSet[Int]() ++ hashedShingles // Convert our Set to a SortedSet
  }

  // Return the Jaccard similarity of two sets
  def compareSets(a: SortedSet[Int], b: SortedSet[Int]): Double = {
    val numerator = a.intersect(b).size.toDouble
    val denominator = a.union(b).size.toDouble
    numerator.toDouble / denominator.toDouble
  }

  // Get the minHash for the shingles
  def minHashing(n: Int, hashedShingles: SortedSet[Int]): List[Int] = {
    // foreach: apply a hash-function to every shingle and select minimum hash and save to list. Repeat for each hash function
    factorList.take(n).map{factor =>
      /* hash function: (ax+b) % c
          a and b are random values
          c is a large prime.
          x is an element from the input set */
      hashedShingles.foldLeft(Int.MaxValue){case (currentMin, hashedShingle) =>
        val hashVal = (Math.abs(factor * hashedShingle) + factor) % 15486557
        Math.min(hashVal, currentMin)
      }
    }
  }

  // Return the fraction equal hashes in two sets.
  def compareSignatures(a: List[Int], b: List[Int]): Double = {
    // Assumption: a & b are of equal size
    val count = a.zip(b).foldLeft(0)((acc: Int, hashTuple: (Int, Int)) => if (hashTuple._1 == hashTuple._2) acc + 1 else acc)
    count.toDouble / a.size.toDouble
  }

  // Return the candidate pairs that will be then compared for signatures.
  def LSH(signatures: List[List[Int]], t: Double): Predef.Set[(Int, Int)] = {

    val numOfBuckets = 6827
    val signatureSize = signatures.head.size

    // Find candidate bands and rows for the amount of hashes. Results in tuples of (b, r) candidates
    val bs = (1 until signatureSize + 1).filter(signatureSize % _ == 0).map(x => (x, signatureSize / x))

    /* Loop through candidates. Accumulator = Tuple(difference, Tuple(b, r))
      Defining r & b: bNr._1 = b , bNr._2 = r
      Equation for estimating t: t = 1/b ^1/r
    */
    val (_, (b, r)) = bs.foldLeft((Double.MaxValue, (0, 0))) { case ((minDiff, (b, r)), bNr) =>
      val tempDiff = Math.abs(t - Math.pow(1 / bNr._1.toDouble, 1 / bNr._2.toDouble))
      if (tempDiff < minDiff) (tempDiff, (bNr._1, bNr._2))
      else (minDiff, (b, r))
    }

    // Iterate from 0 -> amount of signatures with a step size of band size
    (0 until signatureSize by r).flatMap { i =>
      // Reset buckets on each band
      val buckets = new HashMap[Int, Set[Int]] with MultiMap[Int, Int]

      // Go over every document signature for the band
      signatures.indices.foreach { j =>
        val sum = signatures(j).slice(i, i + r - 1).sum
        buckets.addBinding(sum % numOfBuckets, j)
      }

      // For every key in keySet, append list of pairs by getting the values and the possible combinations
      buckets.keySet.map { key =>
        buckets(key).toList.combinations(2).toList.map {
          case List(x, y) => (x, y) // Create combinations of two and turn lists of two elements into tuples
        }.toSet
      }
    }.toSet.flatten
  }
}
