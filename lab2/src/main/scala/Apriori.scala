import scala.io.Source
import scala.collection.mutable.Map

object Apriori {

  // Task 1: Finding frequent itemsets with support at least s
  def readData(path: String): Iterator[String] = Source.fromFile(path).getLines
  val dataPath = "./src/main/scala/T10I4D100K.dat"
  //val dataPath = "./src/main/scala/testDataSlide26.dat"
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

    val filteredTable = table.filter { case (k, v) => (v.toDouble / baskets) >= support }
    val singletons = filteredTable.keySet
    val initialTuples = singletons.flatten.toList.combinations(2).map(x=> x.toSet).toSet
    println("Number of singletons: " + singletons.size)
    (initialTuples, singletons.toSet)
  }

  def launchRecursiveStuff(initial: Set[Set[String]], singletons: Set[Set[String]]): Set[Set[String]] = {
    println("Starting recursive stuff!")
    def recursiveStuff(candidates: Set[Set[String]], singletons: Set[Set[String]], acc: Set[Set[String]]): Set[Set[String]] = {
      candidates.size match {
        case 0 => acc // Base case. No pairs had enough support to be generated.
        case _ =>
          // Scan/filter -> results in a Map[Set[String], count] with elements with support >= supportThreshold
          println("Before scanData")
          val (supportedSets, unSupportedSets) = scanData(readData(dataPath), candidates)
          println("After scanData")
          /* Generate candidates
              Combine supported candidate sets with singletons
              If a singleton already exists in the candidate set -> discard
              If an unsupported set is a subset of the resulting set -> discard
              If result set already exists -> discard
              Else add
           */
          val ourResult = for {
            candidate <- supportedSets
            singleton <- singletons if !singleton.subsetOf(candidate)
            test = candidate | singleton if unSupportedSets.forall(unSupported => !unSupported.subsetOf(test))
          } yield test
          println("Supported set found!")
          recursiveStuff(ourResult.toSet, singletons, acc ++ supportedSets)
      }
    }
    recursiveStuff(initial, singletons, singletons) // arguments are previous, singletons, acc
  }

  // Task 2: Generating association rules with confidence at least c from the itemsets in a sales transaction database (a set of baskets)
  def findAssociation(itemset: Set[Set[String]], confidence: Double): Set[(Set[String], Set[String])] = {
    // For every subset A of itemset I, generate rule A -> I/A (items in itemset I without items in itemset A)
    println("Starting to find associations!")
    val rules = itemset.flatMap(set => set.subsets()
      .filterNot(subset => subset.isEmpty || subset == set) // We are not interest in the empty sets nor the subset == entire set
      .map(subset => (subset, set.diff(subset)))) // (left side of rule, right side of rule)

    /*
        Below, we create a Set[Set[String]] by appending all the left hand side of the rules, i.e. the I in {I -> j},
        to our input parameter itemset which is.
        When scanning through the data again, the occurrence of each item in the newly created set will be counted and put into a Map.
        This Map is used to do lookups when computing the confidence of rules
     */
    val setsToCheckSupport: Set[Set[String]] = itemset ++ rules.map(_._1) // Extract the first element from each tuple and add to itemset
    val supportForRulesAndItems: Map[Set[String], Int] = scanDataOnlyCount(readData(dataPath), setsToCheckSupport)

    /*
        We know each value in the supportForRulesAndItems will exist (don't have to care about Options)
        conf(I -> j) = support(I u j) / support(I)
     */
    rules.filter(rule => supportForRulesAndItems(rule._1.union(rule._2)).toDouble / supportForRulesAndItems(rule._1) >= confidence)
  }

  // scanData counts number of times a candidate occurs and prunes candidates below the support threshold
  def scanData(iter: Iterator[String], candidates: Set[Set[String]]): (Set[Set[String]], Set[Set[String]]) = {
    val result = Map[Set[String], Int]()
    while (iter.hasNext) {
      val data = iter.next()
      candidates
        .withFilter(candidate => candidate.subsetOf(data.split(" ").toSet))
        .foreach(elem => result.put(elem, result.getOrElse(elem, 0) + 1))
    }
    val (supportedSets, unSupportedSets) = result.partition { case (k, v) => (v.toDouble / baskets) >= support }
    (supportedSets.keys.toSet, unSupportedSets.keys.toSet)
  }

  // Like scanData above, but this function only counts the number of times each candidate occurs
  def scanDataOnlyCount(iter: Iterator[String], candidates: Set[Set[String]]): Map[Set[String], Int] = {
    val result = Map[Set[String], Int]()
    while (iter.hasNext) {
      val data = iter.next()
      candidates
        .filter(candidate => candidate.subsetOf(data.split(" ").toSet))
        .foreach(elem => result.put(elem, result.getOrElse(elem, 0) + 1))
    }
    result
  }
}
