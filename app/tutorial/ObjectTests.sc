trait BoardElement
case class Number(value: Int) extends BoardElement
case class Numbers(value: List[Int]) extends BoardElement
object AllNumbers extends Numbers(List.range(1,10))

val t: BoardElement = Number(1)

t match {
  case Numbers(_) => "Match to numbers"
  case Number(_) => "Only Number"
}