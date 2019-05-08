package org.ait.exercises

object MergeSort {
  def main(args: Array[String]): Unit = {
    val nums = List(6, 5, 4, 3, 2, 1)
    println(msort(nums))
    
  }

  def msort(xs: List[Int]): List[Int] = {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
}