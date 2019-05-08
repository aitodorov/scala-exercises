package org.ait.exercises

object NQueens {
  def main(args: Array[String]): Unit = {
    var q = queens(5);
    println(q.map(show))
    //println((0 until 4).map(i=>i))
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        val result = for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
        //println("result: " + result)
        result
      }
    }

    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    println("====== " + queens)
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]) = {
    //val lines = for (col <- queens.reverse)
    //  yield Vector.fill(queens.length)("*").updated(col, "X ").mkString
    //"\n" + (lines mkString "\n") //separate each line by new line
    val lines = "\n" + queens.toString()
    lines
  }
}
