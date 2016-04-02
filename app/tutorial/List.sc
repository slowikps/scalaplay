import scala.collection.mutable.ListBuffer

val oneElem = List(1)

matchList(oneElem)
matchList(Nil)
matchList(List())



def matchList(elem: List[Any]): String = {
  elem match {
    case x :: xs => "x :: xs match"
    case Nil => "Nil"
    case _ => "No idea what is that"
  }
}
0 until 9
List.range(0,9)
List.range(1, 10) map(i => (i, 1))

List(1) match {
  case x :: Nil => "One element"
  case x :: xs => "More than one"
  case Nil => "Empty list"
}

List((1,2)).map(oneElem => oneElem._1)


ListBuffer(1,2,3,4) mkString

"%9s" format(ListBuffer(1,2,3,4) mkString)
val n = 1
"%9s$n"

val s1 = "A"
val s2 = "B"
val s3 = "C"
val padLength = 20
val s = f"$s1%s$s2%20s$s3"


val d = List.range(1, 10).map(in => in -> List(1, 2, 3))


val x = List("a" -> "b", "c" -> "d", "a" -> "f")
x.groupBy(_._1) .map { case (k,v) => (k,v.map(_._2))}