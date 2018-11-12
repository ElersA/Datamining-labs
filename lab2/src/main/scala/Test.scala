object Test extends App {

  val frequentItemSets = Apriori.runStuff(3.0/8)
  println("Frequent itemsets: " + frequentItemSets)

  val rules = Apriori.findAssociation(frequentItemSets, 0.75)
  println("Rules: " + rules)
}
