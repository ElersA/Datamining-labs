
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


