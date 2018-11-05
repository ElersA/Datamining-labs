import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

object SimilarItems {

  // For testing
  val factorList: List[Int] = Source.fromFile("./src/main/scala/factorFile").getLines.toList.map(_.toInt) // From https://www.bigprimes.net/archive/prime/13999999/
  def readFile: List[Char] = Source.fromFile("./tempData.txt").getLines.toList.flatten

  //create shinglings of length k from the list of charachters and return a sorted set contating the hased values for the shinglings
  def shingling(k: Int, document: List[Char]): SortedSet[Int] = {
    val kShingles: Set[List[Char]] = document.sliding(k).toSet
    val hashedShingles = kShingles.map(shingle => Math.abs(shingle.hashCode()))
    SortedSet[Int]() ++ hashedShingles // Convert our Set to a SortedSet
  }

  //Return the Jacard similarty of two sets
  def compareSets(a: SortedSet[Int], b: SortedSet[Int]): Double = {
    val numerator = a.intersect(b).size.toDouble
    val denominator = a.union(b).size.toDouble

    //Check that both sets are not 0
    numerator / denominator match {
      case 0 => throw new IllegalArgumentException("Do not divide by zero")
      case _ => numerator.toDouble / denominator.toDouble
    }
  }

  //Get the minash for the shingles
  def minHashing(n: Int, hashedShingles: SortedSet[Int]): ListBuffer[Int] = {
    val rnd = new Random
    rnd.setSeed(0)
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
      //For each shinle apply the hashfunction save if it is less than what is saved
      hashedShingles.foreach { shingle =>
        var tempmin = (Math.abs(a * shingle) + b) % 15486557
        if (minval > tempmin) {
          minval = tempmin
        }
      }
      listOfMin.append(minval)
      minval = Int.MaxValue
    }
    //Return a set of miniumhashvalues
    println(listOfMin)
    listOfMin
  }

  //Return the fraction from the amount of hashes equal in two sets devieded by the amount of hashes used.
  def compareSignatures(a: ListBuffer[Int], b: ListBuffer[Int]): Double = {
    // Assumption: a & b are of equal size
    val count = a.zip(b).foldLeft(0)((acc: Int, hashTuple: (Int, Int)) => if (hashTuple._1 == hashTuple._2) acc + 1 else acc)
    var i = 0
    //var count =0
    /*while(i<a.size){
      if(a(i) == b(i)){
        count+=1
      }
      i+=1
    }
    */
    println(count)
    count.toDouble / a.size.toDouble
  }

  //Return the canditate pairs that will be then compared for signatures.
  /*
  def LSH(signatures: List[Set[Int]], t: Double): List[(Int, Int)] = {

    val buckets = 10000
    val listOfbAndR = new ListBuffer[(Int,Int)]()

    var b = 1
    //loop and find canditate bands and rows for the amount of hashes
    while({b < signatures(0).size}) {
      val z = signatures(0).size % b
      if (z == 0) {
        listOfbAndR.append((b, signatures(0).size / b))
      }
      b += 1
    }

    //Loop thourgh candidates
    var minTDiff = Double.MaxValue
    var r = 0
    listOfbAndR.foreach{ x =>
      //x._1 = b , x._2 = r
      // t = 1/b ^1/r
      val tempdif = Math.abs(t - Math.pow(1/x._1, 1/x._2))
      if(tempdif<minTDiff){
        minTDiff = tempdif
        b = x._1
        r = x._2
      }
    }

    val candidatePairHolder = Map.empty

    //for each signature get the sum of the band and hash it
    //place the document id into the bucket from the hash
    /*signatures.foreach {sig =>
      val slideList = sig.sliding(r, r).map(_.sum).toList
    }
  */



    //Iterate over the buckets and create pairs of documents
    //return the pairs of documents

    /*
        1. Iterate over each Set in minHashes
        2. For each Set: create a sliding window of size r and step size r
        3. In each sliding window: sum all values and hash the sum
        4. Save the hash
     */
    1
  }
  */
}
