package org.ait.exercises

object TicTacToe {
  def main(args: Array[String]) : Unit = {
    var board : Array[Array[Int]] = Array(Array(0,0,1), Array(0,0,1), Array(0,0,1))
    println(checkBoard(board, 1))
  }

  def checkBoard(board : Array[Array[Int]], player : Int) : Boolean = {
    var rowMatch = false
    var colMatch = false
    var diagMatch1, diagMatch2 = true
    for (i <- 0 until board.length) {
      var rowMatch = true
      var colMatch = true
      for (j <- 0 until board.length) {
        rowMatch = rowMatch && board(i)(j) == player
        colMatch = colMatch && board(j)(i) == player
      }
      if (colMatch || rowMatch)
        return true
      diagMatch1 = diagMatch1 && board(i)(i)==player
      diagMatch2 = diagMatch2 && board(i)(board.length - i -1)==player
    }
    diagMatch1 || diagMatch2
  }
}
