
import scala.io.Source
import scala.util.Random
//import scala.sys.process._

//"ls -al" !

//val a = Source.fromFile("./tempData.txt").getLines.toList.flatten
//a foreach println

val text = List("Hello, world one two three", "five six", "end")
val flattened = text.flatten

val shingles: Set[List[Char]] = flattened.sliding(9).toSet
//println(shingles(0).hashCode())

val hashes = shingles.map(_.hashCode())

val sortedSet = collection.immutable.SortedSet[Int]() ++ hashes
sortedSet foreach println


//val primes = List("15487471	15487933	15488293	15488723")
//primes.map(_.split(" "))

def max(a: Int)(b: Int) = if (a > b) a else b

max(1)(2)


val rnd = new Random
rnd.nextLong


val a = Set(1, 2, 4)
val b = Set(1, 3, 4)



a zip b

val count = a.zip(b).foldLeft(0)((b: Int, a: (Int, Int)) => if (a._1 == a._2) b+1 else b)
println(count)

/*
val lshList = List(Set(1,2,3,4), Set(2,3,4,1), Set(1,3,4,2))
lshList foreach {set =>
  // List where each element is a sum of a band
  val bandSums = set.sliding(2,2).map(_.sum).toList
  val map: Map[Int, String] = bandSums.map(sum => sum % 10000 -> "test") toMap
}
*/









