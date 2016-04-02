import scala.collection.mutable.ListBuffer

trait BoardElement
case class Number(value: Int) extends BoardElement
case class Numbers(value: List[Int]) extends BoardElement
object AllNumbers extends Numbers(List.range(1,10))

val t: BoardElement = Number(1)

t match {
  case Numbers(_) => "Match to numbers"
  case Number(_) => "Only Number"
}


case class NotResolved(val row: Int, val column: Int, var value: ListBuffer[Int])

val nR = NotResolved(1,1, ListBuffer.range(10, 15))

nR match {
  case NotResolved(_, _, l) => l -= 13
}

nR

class Test() {

}