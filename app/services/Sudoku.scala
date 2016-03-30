package services

/**
  * Created by slowikps on 19/03/16.
  */
object Sudoku {

  def main(args: Array[String]) {
    val b = new Board("GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG")
    print(b)
  }
}

class Board {
  var matrix: Array[Array[BoardElement]] = Array.ofDim[BoardElement](9, 9)


  def this(str: String) = {
    this()
    str.zipWithIndex
      .foreach {
        case (c, i) => matrix(i / 9)(i % 9) = if (c.isDigit) Number(c - 48) else AllNumbers
      }
  }

  override def toString = {
    (
      for {
        row <- 0 until matrix.length
        col <- 0 until matrix.length
        toPrint <- matrix(row)(col) match {
          case Number(n) => n toString
          case Numbers(_) => "*"
        }
      } yield if (col == 0) "\n" + toPrint else toPrint
      ) mkString (" ")
  }
}

trait BoardElement

case class Number(value: Int) extends BoardElement

case class Numbers(value: List[Int]) extends BoardElement

object AllNumbers extends Numbers(List.range(1, 10))