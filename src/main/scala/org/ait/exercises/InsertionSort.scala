package org.ait.exercises

object InsertionSort {
  def main(args: Array[String]): Unit = {
    val l = List(4, 6, 23, 6, 7)
    val sortedList = sort(l)
    for (i <- sortedList)
      print(i + " ")
  }

  def sort(list: List[Int]): List[Int] = {
    
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case List()  => List(x)
      case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }

    list match {
      case List()  => List()
      case y :: ys => insert(y, sort(ys))
    }
  }

}