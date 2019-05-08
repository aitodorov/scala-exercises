package org.ait.exercises

object ReverseList {

  def main(args: Array[String]) : Unit = {
    reverseList(List())
    reverseList(List(1, 2, 3, 4, 5))
    reverseList(List("A", "B", "C", "D", "E", "F"))

  }

  def reverseList1[T](list : List[T]) : List[T] = {
    def _reverse[T](reversed : List[T], list : List[T]) : List[T] = list match {
        case Nil => reversed
        case head::tail => _reverse(head::reversed, tail)
    }
    println("List to be reversed: " + list)
    val reversed = _reverse(Nil,list)
    println("ReverseList: " + reversed)
    reversed
  }

  def reverseList[T](list : List[T]) : List[T] = {
    println("\nList to be reversed: " + list)
    val r = list.foldLeft(List[T]())((reversed,element)=>element :: reversed)
    println("ReverseList: " + r)
    r
  }

}
