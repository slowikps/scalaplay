//Immutable

val originalList = List(5, 1, 4, 3, 2)
//originalList -= 5  NOT DEFINED FOR IMMUTABLE
val newList = originalList.filter(_ > 2)


//Mutable
import scala.collection.mutable.ListBuffer

val x = ListBuffer("First", "Second", "Third", "4th", "5th", "6th")
x -= ("First", "5th")
x.remove(0)
x

val toRemove = List("Third", "4th")

x --= toRemove

println("The end")


//Creation:
val fruits = new Array[String](3)


var board: Array[Array[Int]] = Array.ofDim[Int](9, 9)
"GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG".zipWithIndex
  .foreach {
    case (c, i) => board(i / 9)(i % 9) = if(c.isDigit) c - 48 else 0
  }


for (i <- 0 to 8) {
  for ( j <- 0 to 8) {
    print(" " + board(i)(j));
  }
  println();
}

"Map test"
board flatMap(elem => elem)
"End"