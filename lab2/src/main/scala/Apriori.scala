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

  val filteredTable = table.filter { case (k, v) => (v.toDouble / baskets) >= support }
  table.keySet.toList.combinations(2)

  def launchRecursiveStuff(initial: Set[Set[String]]): Set[Set[String]] = {
    def recursiveStuff(previous: Set[Set[String]], singletons: Set[Set[String]], acc: Set[Set[String]]): Set[Set[String]] = {

      /*
          1. Scan/filter -> results in a Map[Set[String], count] with elements with support >= supportThreshold
          2. Generate candidates
       */
      val (supportedSets, unSupportedSets) = scanData(readData(dataPath), previous)

      previous match {
        case Set.empty => acc // Base case. No pairs had enough support to be generated.
        case _ =>
          /*
              Combine supported candidate sets with singletons
              If a singleton already exists in the candidate set -> discard
              If an unsupported set is a subset of the resulting set -> discard
              If result set already exists -> discard
              Else add

           */
          val ourResult = for {
            candidate <- supportedSets
            singleton <- singletons
            if !singleton.subsetOf(candidate)
            test = candidate | singleton

            if unSupportedSets.forall(unSupported => !unSupported.subsetOf(test))
            //if supportedSets.exists(supported => supported.subsetOf(test))
          } yield test
          ourResult.toSet
      }
    }
    recursiveStuff(initial, initial, initial) // arguments are previous, singletons, acc
  }

  def scanData(iter: Iterator[String], elems: Set[Set[String]]): (Set[Set[String]], Set[Set[String]]) = {
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
    val (supportedSets, unSupportedSets) = result.partition { case (k, v) => (v.toDouble / baskets) >= support }
    (supportedSets.keys.toSet, unSupportedSets.keys.toSet)
  }


  // Task 2: Generating association rules with confidence at least c from the itemsets in a sales transaction database (a set of baskets)


}
