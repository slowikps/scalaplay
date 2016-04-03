package services
import scala.collection.mutable.ListBuffer

/**
  * Created by slowikps on 19/03/16.
  */
abstract class BoardElement(val row: Int, val column: Int)

case class Resolved(override val row: Int, override val column: Int, value: Int) extends BoardElement(row, column) {
  def this(nR: NotResolved, value: Int) = this(nR.row, nR.column, value)
}

case class NotResolved(override val row: Int, override val column: Int, var value: ListBuffer[Int]) extends BoardElement(row, column)

case class RelativeElements(val row: List[BoardElement], val col: List[BoardElement], val square: List[BoardElement]) {
  def getAllFlat: List[BoardElement] = row ::: col ::: square
  def getAll = List(row, col, square)
}

object Sudoku {

  def main(args: Array[String]) {
    val result = new Board("675349128248176359193852764716483592524917836839625471987561243462738915351294687")
    val b = new Board("FGE34IABH2DHA7FC5IAI38EB764G1F4HC592EBDIAGHCF839FB5D7A987EF12DCD6BG3HIA5CEAB94FHG", result)
    print("Start: \n" + b + "\n")
    b.start()
    println("End: \n" + b)

    print(b.toString.replaceAll("\n", "").replaceAll(" ", ""))
  }
}
//Refactor this class - too many public variables - used in test but need to be fixed
class Board(result: Option[Board]) {
  var matrix: Array[Array[BoardElement]] = Array.ofDim[BoardElement](9, 9)

  //TODO - I don't like it
  def this(str: String, result: Board) {
    this(Option(result))
    str.zipWithIndex
      .foreach {
        case (c, i) => matrix(i / 9)(i % 9) = if (c.isDigit) Resolved(i / 9, i % 9, c - 48) else NotResolved(i / 9, i % 9, ListBuffer.range(1, 10))
      }
  }

  def this(str: String) = {
    //TODO why Nil doesn't work here?
    this(str, null)
  }

  def start() = {
    step(resolveInitialNumbers)
  }

  def processElement(x: Resolved) = {
    getAllElementsInRelation(x).getAllFlat flatMap {
      case r : Resolved => None //already resolved
      case nR : NotResolved => {
        nR.value -= x.value
        if(nR.value.length == 1) {
          //TODO: I don't like this
          Some(toResolved((nR.value.head -> List(nR))))
        } else {
          None
        }
      }
    }
  }

  private def step(resolved: List[Resolved]): Unit = {
    resolved match {
      case x :: xs => step(xs ::: processElement(x))
      case _ => {
        val newResolved: Array[Resolved] = matrix.flatMap(list => list).flatMap(elem => {
          val inRelation = getAllElementsInRelation(elem)

          val wielkaDupa: List[Resolved] = inRelation.getAll.flatMap((relatives: List[BoardElement]) => {
            //TODO: if there is no results this could/should be detected here!
            relatives.flatMap {
              case n: NotResolved => n.value map (number => (number -> n))
              case _ => None
            }.groupBy(_._1).map{
              case (k,v) => (k, v.map(_._2))
            }.filter(tup => tup._2.length == 1).map(toResolved)
          }
          )

          wielkaDupa
        })
        if(newResolved nonEmpty) step(newResolved.toList)
      }
    }
  }

  def toResolved(f: (Int, List[NotResolved])): Resolved = {
      f match {
        case (k, v) => {
          val resolved: Resolved = new Resolved(v.head, k)
          result.foreach(r => {
            val shouldBe = r.matrix(resolved.row)(resolved.column)
            if(shouldBe != resolved) throw new RuntimeException(s"suggested element value: $resolved doesn't match result: $shouldBe")
          })
          matrix(resolved.row)(resolved.column) = resolved
          resolved
        }
      }
  }

  private def resolveInitialNumbers: List[Resolved] = {
    matrix.flatMap(elem => elem)
      .toList
      .flatMap {
        case x : Resolved => Some(x)
        case _ => None
      }
  }

  def getAllElementsInRelation(elem: BoardElement) = {
    def getSameRowElements = List.range(0, 9) map (indx => (elem.row, indx))

    def getSameColumnElements = List.range(0, 9) map (indx => (indx, elem.column))

    def getSameSquareElements = {
      val vek = (elem.row / 3 * 3, elem.column / 3 * 3)
      List.range(0,9) map (elem => (elem / 3 + vek._1, elem % 3 + vek._2))
    }

    def toBoardElement(elems: List[(Int, Int)]) = elems map(point => matrix(point._1)(point._2))

    new RelativeElements(toBoardElement(getSameRowElements), toBoardElement(getSameColumnElements), toBoardElement(getSameSquareElements))
  }

  override def toString = {
    (
      for {
        row <- 0 until matrix.length
        col <- 0 until matrix.length
        toPrint <- matrix(row)(col) match {
          case Resolved(_, _, n) => n toString
          case NotResolved(_, _, l) if (l.length > 1) =>  "*"
        }
      } yield if (col == 0) "\n" + toPrint else toPrint
      ) mkString " "
  }
}



