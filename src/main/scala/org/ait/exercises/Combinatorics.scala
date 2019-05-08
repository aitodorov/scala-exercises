package org.ait.exercises

object Combinatorics {
  def main(args: Array[String]): Unit = {
    println("possible prime sums in " + 7);
    println(listPrimeSums(7))

    /*var c1 = (for {
      i <- 1 to 4
      j <- 1 until i
      k <- 1 until j
    } yield List(i, j, k))
    println(c1)*/
    println("combinations");
    println(combinations(4,3))

    println(3 + " permutations");
    println(permutations(3).map(show))

    println(5 + " queens");
    println(queens(5).map(show))
  }

  /**
    *
    * @param n
    * @param k
    * @return
    */
  def combinations(n : Int, k : Int): Seq[List[Int]] = {
    if (k<=0 || n<=0) null

    def helper(n : Int, pos : Int, accum : Seq[List[Int]]): Seq[List[Int]] = {
      val listsPosElem = for {
        list <- accum
        j <- 1 until list.last
      } yield list ++ List(j)

      if (pos == k) {
        listsPosElem
      } else {
        helper(n, pos + 1, listsPosElem)
      }
    }

    val lists1Elem : Seq[List[Int]] = for {
      i <- 1 to n
    } yield List(i)

    if (k == 1)
      lists1Elem
    else
      helper(n,2,lists1Elem)
  }

  /**
    *
    * @param n
    * @return
    */
  def listPrimeSums(n: Int): Seq[(Int, Int)] = {
    def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

    (1 until n).flatMap(
      i=> (i + 1 until n).map( j=>(i,j) )).filter(
      pair => isPrime(pair._1 + pair._2))
  }

  def listPrimeSumsYield(n: Int): Seq[(Int, Int)] = {
    def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

    for {
      i <- 1 until n
      j <- i + 1 until n
      if isPrime(i + j)
    } yield (i, j)
  }

  /**
    *
    * @param n
    * @return
    */
  def permutations(n : Int): Seq[List[Int]] = {
    if (n<=0) null

    def helper(k : Int, accum : Seq[List[Int]]): Seq[List[Int]] = {
      val kPermutations = for {
        list <- accum
        i <- 1 to n
        if (!list.contains(i))
      } yield list ++ List(i)

      if (k == n) {
        kPermutations
      } else {
        helper(k+1, kPermutations)
      }
    }

    val firstPosPerms : Seq[List[Int]] = for {
      i <- 1 to n
    } yield List(i)

    if (n == 1)
      firstPosPerms
    else
      helper(2,firstPosPerms)
  }

  /**
    *
    * @param n
    * @return
    */
  def queens(n : Int): Seq[List[Int]] = {
    if (n<=0) null

    def isSafe(newQueenRow : Int, queens : List[Int]) : Boolean = {
      val newQueenCol =  queens.length
      val queensWithCol = queens zip (0 until queens.length)
      queensWithCol forall {
        case (row, col) => newQueenRow != row && math.abs(newQueenRow - row) != newQueenCol - col
      }
    }

    def helper(k : Int, accum : Seq[List[Int]]): Seq[List[Int]] = {
      val kQueens = for {
        queens <- accum
        i <- 0 until n
        if (isSafe(i, queens))
      } yield queens ++ List(i)

      if (k == n) {
        kQueens
      } else {
        helper(k+1, kQueens)
      }
    }

    val firstPosQueens : Seq[List[Int]] = for {
      i <- 0 until n
    } yield List(i)

    if (n == 1)
      firstPosQueens
    else
      helper(2,firstPosQueens)
  }

  def show(list : List[Int]) = {
    "\n" + list.toString()
  }
}
