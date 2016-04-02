package services

import org.specs2.mutable._

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


  }
}