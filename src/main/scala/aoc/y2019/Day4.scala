package aoc.y2019

object Day4 {
  private def (i: Int) digits: Seq[Int] = 
    LazyList.iterate(Some(i): Option[Int]) { 
      case Some(i) if i > 0 => Some(i / 10)
      case _ => None
    }
      .takeWhile(_.isDefined)
      .flatten
      .map(_ % 10)
      .toList
      .reverse

  def solve(input: String): Int = {
    val range = input.split("-").map(_.toInt)
    (for {
      pass <- range(0) to range(1)
      passDigits = pass.digits
      if passDigits.sliding(2, 1).exists { pair => pair.head == pair.tail.head }
      if passDigits.sliding(2, 1).forall { pair => pair.head <= pair.tail.head }
    } yield pass)
      .size
  }

  def solve2(input: String): Int = {
    val range = input.split("-").map(_.toInt)
    (for {
      pass <- range(0) to range(1)
      passDigits = pass.digits
      if passDigits.foldRight(Nil: List[(Int, Int)]) { (digit, acc) =>
        acc match {
          case (d, cnt) :: tail if d == digit => (digit, cnt + 1) :: tail
          case _ => (digit, 1) :: acc
        }
      }.exists((_, cnt) => cnt == 2)
      if passDigits.sliding(2, 1).forall { pair => pair.head <= pair.tail.head }
    } yield pass)
      .size
  }
  
  val input = "264360-746325"
}