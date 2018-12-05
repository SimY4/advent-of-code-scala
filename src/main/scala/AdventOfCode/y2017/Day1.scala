package AdventOfCode
package y2017

import scala.annotation.tailrec

object Day1 {

  def sum(str: String): Int = {
    val digitsList = str.map(_.asDigit).toList
    val first = digitsList.head

    @tailrec def sum0(acc: Int, seq: Seq[Int]): Int = seq match {
      case x :: Nil if x == first       => acc + x
      case _ :: Nil                     => acc
      case x1 :: x2 :: rest if x1 == x2 => sum0(acc + x1, x2 :: rest)
      case _ :: rest                    => sum0(acc, rest)
    }

    sum0(0, digitsList)
  }

  println(sum("1234") == 0)
  println(sum("1111") == 4)
  println(sum("1122") == 3)

  // PART 2

  def sum2(str: String): Int = {
    val digitsList = str.map(_.asDigit).toList

    @tailrec def sum20(acc: Int, idx: Int, scoutIdx: Int): Int =
      if (idx < digitsList.size) {
        val x = digitsList(idx)
        val nextScoutIdx = (scoutIdx + 1) % digitsList.size
        if (x == digitsList(scoutIdx)) {
          sum20(acc + x, idx + 1, nextScoutIdx)
        } else {
          sum20(acc, idx + 1, nextScoutIdx)
        }
      } else {
        acc
      }

    sum20(0, 0, digitsList.size / 2)
  }

  println(sum2("1212") == 6)
  println(sum2("1221") == 0)
  println(sum2("123425") == 4)
  println(sum2("123123") == 12)
  println(sum2("12131415") == 4)

}
