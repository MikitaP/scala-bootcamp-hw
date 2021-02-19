package basics

object Collections extends App {

  // https://leetcode.com/problems/running-sum-of-1d-array/
//  val list = List(1, 2, 3, 5)

  def runningSum(list: List[Int]): Either[String, List[Int]] = {
    if (list.nonEmpty
        && list.sum >= math.pow(10, 6) * -1
        && list.sum <= math.pow(10, 6))
      Right(list.scanLeft(0)(_ + _).tail)
    else
      Left("Constraints failed")

  }

  // https://leetcode.com/problems/shuffle-the-array

//  val list = List(2, 5, 1, 3, 4, 7)
  def shuffleOfArray(list: List[Int], n: Int): Either[String, List[Int]] = {
    if (n >= 1 && n <= 500
        && list.size == n * 2
        && list.sum >= 1
        && list.sum <= math.pow(10, 3))
      Right(
        list
          .slice(0, n)
          .zip(list.slice(n, list.size))
          .flatten(tup => List(tup._1, tup._2))
      )
    else
      Left("Constraints failed")
  }

  // https://leetcode.com/problems/richest-customer-wealth

  //  val list = List(List(1, 2, 3), List(3, 2, 1))

  def richestCustomerWealth(account: List[List[Int]]): Int = {
    account.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/

//  val list = List(2, 3, 5, 1, 3)

  def kidsWithCandies(candies: List[Int], extraCandies: Int): List[Boolean] = {
    candies.map(i => i + extraCandies > candies.max)
  }

//  https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points/

//  val list =
//    List((3, 1), (9, 0), (1, 0), (1, 4), (5, 3), (8, 8))

  def maxWidthOfVerticalArea(points: List[(Int, Int)]): Int = {
    points
      .map(x => x._1)
      .sorted
      .distinct
      .zip(points.map(x => x._1).sorted.distinct.tail)
      .map(x => x._2 - x._1)
      .max
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    s.foldLeft(List.empty[(Char, Int)]) {
        case ((last, count) :: tail, c) if c == last =>
          (last, count + 1) :: tail
        case (acc, c) =>
          (c, 1) :: acc
      }
      .reverse
  }

  def findGap(l: List[Int]): Option[(Int, Int)] = l match {
    case Nil     => None
    case _ :: xs => (l zip xs).find { case (x, y) => y - x != 1 }
  }

//  def min(list: List[Int]): Option[Int] = list match {
//    case Nil => None
//    case _   => list
//  }

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] =
    list
      .foldLeft(List(zero))((acc, elem) => f(acc.head, elem) :: acc)
      .reverse
}
