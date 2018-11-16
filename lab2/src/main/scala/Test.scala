object Test extends App {

  val frequentItemSets = Apriori.getItemsets(0.01)
  println("Frequent itemsets:")
  frequentItemSets.filterNot(_.size == 1).foreach(itemset => println(itemset)) // filterNot to remove singleton printouts (easier to read)

  val rules = Apriori.findAssociation(frequentItemSets, 0.3)
  println("Association rules:")
  rules.foreach(rule => println(rule._1.mkString("{", ",", "}") + " -> " + rule._2.mkString("{", ",", "}")))
}
