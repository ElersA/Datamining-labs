import scala.io.Source

object Apriori {

  // Task 1: Finding frequent itemsets with support at least s
  val lines = Source.fromFile("./T10I4D100K.dat").getLines
  var table = scala.collection.mutable.Map[Int, Int]()
  val support = 0.01
  val baskets = lines.size

  while (lines.hasNext) {
    val data = lines.next()
    data.split(" ").foreach(elem => table.put(elem.toInt, table.getOrElse(elem.toInt, 0) + 1))
  }

  table.filter{case (k,v) => (v / baskets) >= support}




  // Task 2: Generating association rules with confidence at least c from the itemsets in a sales transaction database (a set of baskets)



}
