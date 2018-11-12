import scala.io.Source
import scala.collection.mutable.Map

object Apriori {

  // Task 1: Finding frequent itemsets with support at least s
  def readData(path: String): Iterator[String] = Source.fromFile(path).getLines
  val dataPath = "./src/main/scala/testDataSlide43.dat"
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
  //maybe add support value in tuple for frequent itemsets to be able to use that value here
  def findAssociation(itemset: Set[Set[String]], confidence: Double) = {
    //For every subset A of in itemset generate rule A->I/A(items let in itemset a without items A)
    val rules = itemset.flatMap(set => set.subsets()
      .filterNot(subset => subset.isEmpty || subset == set)
      .map(subset => (subset, set.diff(subset)))) // Set[(Set[String], Set[String])]

    //need to union the asso sets and the given itemset and call scandata to get the support for the diffrent rules in a ahas map
    //the support for rulres are then used to cpmpute confidence
    //if the confidence is higher than the paramter given save the rules in a set
    //after every rules has been tested return the set with all rules with enough confidence

    val setsToCheckSupport: Set[Set[String]] = itemset ++ rules.map(_._1) // Extract the first element from each tuple and add to itemset

    val supportForRulesandItemst: Map[Set[String], Int] = scanDataOnlyCount(readData(dataPath), setsToCheckSupport)

    /*
        We know each value in the supportForRulesandItemst will exist (don't have to care about Options)
        conf(I -> j) = support(I u j) / support(I)
        conf(supportForRulesandItemst(x._1)) =
          support(supportForRulesandItemst(x._1) union supportForRulesandItemst(x._2))
          / support(supportForRulesandItemst(supportx._1))
        is this above the threshold we have as parameter?
     */

    rules.filter(rule => supportForRulesandItemst(rule._1.union(rule._2)) / supportForRulesandItemst(rule._1) >= confidence)
/*
    val assorules = Set[(Set[String],Set[String])]
    rules.foreach( x=>
      if(scanDataOnlyCount())


    )
    itemset.map(set=> set.subsets())
    //if rule A-> I/A is below given confidence then subsets of A rules will allso be
    //can thus generate bigger rules from smaller ones..*/
  }

  def scanDataOnlyCount(iter: Iterator[String], candidates: Set[Set[String]]): Map[Set[String], Int] = {
    /*
        1. Counts
    */
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
