object Test extends App {
  val first = Apriori.firstPass(0.5)
  println(first)

  Apriori.launchRecursiveStuff(first)
}
