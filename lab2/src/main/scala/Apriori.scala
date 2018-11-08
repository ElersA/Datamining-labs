import scala.io.Source
import scala.collection.mutable.Map

object Apriori {

  // Task 1: Finding frequent itemsets with support at least s
  def readData(path: String): Iterator[String] = Source.fromFile(path).getLines
  val dataPath = "./T10I4D100K.dat"
  val lines = readData(dataPath)
  val support = 0.01
  val table = Map[Set[String], Int]()

  var counter = 0
  while (lines.hasNext) {
    counter += 1
    val data = lines.next()
    data.split(" ").foreach(elem => table.put(Set(elem), table.getOrElse(Set(elem), 0) + 1))
  }
  val baskets = counter


  def scanData(iter: Iterator[String], elems: List[Set[String]]): List[(Set[String], Set[String])] = {
    /*
        1. Counts
        2. Prunes
     */
    val result = Map[Set[String], Int]()
    while (iter.hasNext) {
      val data = iter.next()
      elems
        .filter(candidate => candidate.subsetOf(data.split(" ").toSet))
        .foreach(elem => result.put(elem, result.getOrElse(elem, 0) + 1))
    }
    result
  }

  val filteredTable = table.filter { case (k, v) => (v.toDouble / baskets) >= support }

  table.keySet.toList.combinations(2)

  def launchRecursiveStuff(initial: Map[Set[String], Int]): List[Set[Int]] = {
    def recursiveStuff(current: Map[Set[String], Int], previous: List[Set[String]], singletons: Set[String]): List[Set[String]] = {

      /*
          1. Scan/filter -> results in a Map[Set[String], count] with elements with support >= supportThreshold
            scanData(readData(path), previous)

          2. Generate candidates
            for {
              candidatePair <-
              if ()
            } yield

       */


      current match {
        case Map.empty => previous // Base case. No pairs had enough support to be generated.
        case _ =>
      }
    }

    recursiveStuff(initial, List(initial))
  }


  // Task 2: Generating association rules with confidence at least c from the itemsets in a sales transaction database (a set of baskets)


}
