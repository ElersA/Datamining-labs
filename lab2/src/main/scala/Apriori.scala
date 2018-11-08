import scala.io.Source
import scala.collection.mutable.Map

object Apriori {

  // Task 1: Finding frequent itemsets with support at least s
  def readData(path: String): Iterator[String] = Source.fromFile(path).getLines
  val dataPath = "./src/main/scala/testData.dat"
  var baskets: Int = 0
  var support: Double = 0

  def runStuff(supprt: Double): Set[Set[String]] = {
    support = supprt
    val (initialTuples, singletons)= firstPass() // Initial is C2 on slide 43 in lecture 3
    launchRecursiveStuff(initialTuples, singletons)
  }

  def firstPass(): (Set[Set[String]], Set[Set[String]]) = {

    val lines = readData(dataPath)
    val table = Map[Set[String], Int]()

    var counter = 0
    while (lines.hasNext) {
      counter += 1
      val data = lines.next()
      data.split(" ").foreach(elem => table.put(Set(elem), table.getOrElse(Set(elem), 0) + 1))
    }
    baskets = counter
    //println(table)
    val filteredTable = table.filter { case (k, v) => (v.toDouble / baskets) >= support }
    val singletons = filteredTable.keySet
    val initialTuples = singletons.flatten.toList.combinations(2).map(x=> x.toSet).toSet
    (initialTuples, singletons.toSet)
  }

  def launchRecursiveStuff(initial: Set[Set[String]], singletons: Set[Set[String]]): Set[Set[String]] = {
    def recursiveStuff(candidates: Set[Set[String]], singletons: Set[Set[String]], acc: Set[Set[String]]): Set[Set[String]] = {

      /*
          1. Scan/filter -> results in a Map[Set[String], count] with elements with support >= supportThreshold
          2. Generate candidates
       */

      candidates.size match {
        case 0 => acc // Base case. No pairs had enough support to be generated.
        case _ =>

          val (supportedSets, unSupportedSets) = scanData(readData(dataPath), candidates)
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
          recursiveStuff(ourResult.toSet, singletons, acc ++ supportedSets)
      }
    }
    recursiveStuff(initial, singletons, singletons) // arguments are previous, singletons, acc
  }

  def scanData(iter: Iterator[String], candidates: Set[Set[String]]): (Set[Set[String]], Set[Set[String]]) = {
    /*
        1. Counts
        2. Prunes
     */
    val result = Map[Set[String], Int]()
    while (iter.hasNext) {
      val data = iter.next()
      candidates
        .filter(candidate => candidate.subsetOf(data.split(" ").toSet))
        .foreach(elem => result.put(elem, result.getOrElse(elem, 0) + 1))
    }
    val (supportedSets, unSupportedSets) = result.partition { case (k, v) => (v.toDouble / baskets) >= support }
    (supportedSets.keys.toSet, unSupportedSets.keys.toSet)
  }


  // Task 2: Generating association rules with confidence at least c from the itemsets in a sales transaction database (a set of baskets)


}
