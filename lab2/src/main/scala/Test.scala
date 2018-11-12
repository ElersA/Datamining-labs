object Test extends App {

  val frequentItemSets = Apriori.runStuff(0.04)
  println("Frequent itemsets: " + frequentItemSets)

  val rules = Apriori.findAssociation(frequentItemSets, 0.75)
  println("Rules: " + rules)
}
