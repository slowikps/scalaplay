val p = (1, 5)

val vek = (1 / 3 * 3, 4 / 3 * 3)

(0 until 9) map (elem => (elem / 3 + vek._1, elem % 3 + vek._2))



def getAllElementsInRelation(row: Int, col: Int) = {
  def getSameRowElements = List.range(0, 9) map (indx => (row, indx))

  def getSameColumnElements = List.range(0, 9) map (indx => (indx, col))

  def getSameSquareElements = {
    val vek = (row / 3 * 3, col / 3 * 3)
    List.range(0, 9) map (elem => (elem / 3 + vek._1, elem % 3 + vek._2))
  }

  getSameRowElements ::: getSameColumnElements ::: getSameSquareElements
}

getAllElementsInRelation(0, 0)


def callF(f: (Int, String) => String) = f(11, "Test")


callF((a, b) => s"The params are, int: $a and str: $b")

//Null is considered value:
Some(null)
//But Option is smarter:
val d = Option(null)