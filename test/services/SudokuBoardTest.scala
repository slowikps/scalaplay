package services

import org.specs2.matcher.MatchResult
import org.specs2.mutable._

import scala.collection.immutable.IndexedSeq

/**
  * Created by slowikps on 31/03/16.
  */
class SudokuBoardTest extends Specification {

  //  "The 'Hello world' string" should {
  //    "contain 11 characters" in {
  //      "Hello world" must have size(11)
  //    }
  //    "start with 'Hello'" in {
  //      "Hello world" must startWith("Hello")
  //    }
  //    "end with 'world'" in {
  //      "Hello world" must endWith("world")
  //    }
  //  }

  "Sudoku board" should {
    "accept string in constructor" in {
      new Board("GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG") must haveClass[Board]
    }
    "create correct bord from string" in {
      val input: String = "GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG"
      val board: Board = new Board(input)
      input.zipWithIndex foreach {
        case (c, idx) if c.isDigit => {
          board.matrix(idx / 9)(idx % 9) match {
            case r : Resolved => r.value mustEqual c.asDigit
            case _ => failure
          }
        }
        case _ =>
      }

      input must haveClass[String]
    }
    "return correct relatives elements for (0,0) element" in {
      val board = new Board("GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG")
      val toValidate: RelativeElements = board.getAllElementsInRelation(board.matrix(0)(0))
      val inRowExpected: List[(Int, Int)] = List.range(0, 9).map(elem => (0, elem))
      val inColumnExpected: List[(Int, Int)] = List.range(0, 9).map(elem => (elem, 0))
      val inSquareExpected = for {
        row <- 0 until 3
        col <- 0 until 3
      } yield (row, col)

      toValidate.row.map(e => (e.row, e.column)) must containTheSameElementsAs(inRowExpected)
      toValidate.col.map(e => (e.row, e.column)) must containTheSameElementsAs(inColumnExpected)
      toValidate.square.map(e => (e.row, e.column)) must containTheSameElementsAs(inSquareExpected)
    }

    "return correct relatives elements for (4,4) element" in {
      val testRow = 4
      val testCol = 4

      val board = new Board("GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG")
      val toValidate: RelativeElements = board.getAllElementsInRelation(board.matrix(testRow)(testCol))
      val inRowExpected: List[(Int, Int)] = List.range(0, 9).map(elem => (testRow, elem))
      val inColumnExpected: List[(Int, Int)] = List.range(0, 9).map(elem => (elem, testCol))
      val inSquareExpected = for {
        row <- 0 until 3
        col <- 0 until 3
      } yield (row + 3, col + 3)

      toValidate.row.map(e => (e.row, e.column)) must containTheSameElementsAs(inRowExpected)
      toValidate.col.map(e => (e.row, e.column)) must containTheSameElementsAs(inColumnExpected)
      toValidate.square.map(e => (e.row, e.column)) must containTheSameElementsAs(inSquareExpected)
    }

    "return correct relatives elements for (8,8) element" in {
      val testRow = 8
      val testCol = 8

      val board = new Board("GAHC4F9EBF5I721HC4CDBHIEG16I64AH25GCHC79E46BAAB56GC48I27ADFHCIE4IC517B6HEH6B3IADG")
      val toValidate: RelativeElements = board.getAllElementsInRelation(board.matrix(testRow)(testCol))
      val inRowExpected: List[(Int, Int)] = List.range(0, 9).map(elem => (testRow, elem))
      val inColumnExpected: List[(Int, Int)] = List.range(0, 9).map(elem => (elem, testCol))
      val inSquareExpected = for {
        row <- 0 until 3
        col <- 0 until 3
      } yield (row + 6, col + 6)

      toValidate.row.map(e => (e.row, e.column)) must containTheSameElementsAs(inRowExpected)
      toValidate.col.map(e => (e.row, e.column)) must containTheSameElementsAs(inColumnExpected)
      toValidate.square.map(e => (e.row, e.column)) must containTheSameElementsAs(inSquareExpected)
    }
  }
}