import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer
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
  def minHashing(n: Int, hashedShingles: SortedSet[Int]): ListBuffer[Int] = {

    var minval = Int.MaxValue
    val listOfMin = new ListBuffer[Int]()
    // foreach hash apply to every shingle get min value for that hash and save to list
    factorList.take(n).foreach { factor =>
      /*
        hash function: (ax+b) % c
          a and b are random values
          c is a large prime.
          x is an element from the input set
      */
      val a = factor
      val b = factor
      //For each shingle apply the hash function save if it is less than what is saved
      hashedShingles.foreach { shingle =>
        val tempmin = (Math.abs(a * shingle) + b) % 15486557
        if (minval > tempmin) {
          minval = tempmin
        }
      }
      listOfMin.append(minval)
      minval = Int.MaxValue
    }

    listOfMin // Return a set of minimum hash values
  }

  // Return the fraction equal hashes in two sets.
  def compareSignatures(a: ListBuffer[Int], b: ListBuffer[Int]): Double = {
    // Assumption: a & b are of equal size
    val count = a.zip(b).foldLeft(0)((acc: Int, hashTuple: (Int, Int)) => if (hashTuple._1 == hashTuple._2) acc + 1 else acc)
    count.toDouble / a.size.toDouble
  }

  // Return the candidate pairs that will be then compared for signatures.
  def LSH(signatures: List[ListBuffer[Int]], t: Double): Set[(Int, Int)] = {

    val numOfBuckets = 6827
    val listOfbAndR = new ListBuffer[(Int, Int)]()
    val sigSize = signatures.head.size
    var b = 1

    // loop and find candidate bands and rows for the amount of hashes
    while (b < sigSize + 1) {
      val z = sigSize % b
      if (z == 0) {
        listOfbAndR.append((b, sigSize / b))
      }
      b += 1
    }

    // Loop through candidates
    var minTDiff = Double.MaxValue
    var r = 0
    listOfbAndR.foreach { x =>
      // Defining r & b: x._1 = b , x._2 = r
      // Equation for estimating t: t = 1/b ^1/r
      val tempdif = Math.abs(t - Math.pow(1 / x._1.toDouble, 1 / x._2.toDouble))
      //println(tempdif)
      //println("r:" + x._2 + "b " + x._1)

      if (tempdif < minTDiff) {
        minTDiff = tempdif
        b = x._1
        r = x._2
      }
    }
    println("bands " + b + " rows " + r)

    var setOfPairs = Set[(Int, Int)]()
    var buckets = new HashMap[Int, Set[Int]] with MultiMap[Int, Int]

    var i = 0
    var j = 0

    // Iterate from 0 -> amount of signatures by adding band size
    while (i < sigSize) {

      // Go over every document signature for the band
      while (j < signatures.length) {
        // Get the sum for the document and hash it and save the document index
        val sum = signatures(j).slice(i, i + r - 1).sum
        buckets.addBinding(sum % numOfBuckets, j)
        j += 1
      }
      // For every key in key set append list of paris by geting the values and the posible combinations
      for (key <- buckets.keySet) {
        // Create combinations of two and turn lists of two elements into tuples
        setOfPairs = setOfPairs.union(buckets(key).toList.combinations(2).toList.map { case List(x, y) => (x, y) }.toSet)
      }
      // Rest buckets and move to next band
      buckets = new HashMap[Int, Set[Int]] with MultiMap[Int, Int]
      j = 0
      i += r
    }

    setOfPairs // Return the set of pairs
  }
}
