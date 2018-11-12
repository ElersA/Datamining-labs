object Test extends App {

  val frequentItemSets = Apriori.getItemsets(3.0/8)
  println("Frequent itemsets:")
  frequentItemSets.foreach(itemset => println(itemset))

  val rules = Apriori.findAssociation(frequentItemSets, 0.75)
  println("Association rules:")
  rules.foreach(rule => println(rule._1.mkString("{", ",", "}") + " -> " + rule._2.mkString("{", ",", "}")))
}
