package aoc
package y2025

object Day1 extends Input(2025, 1):
  def solve(input: String): Int =
    input.linesIterator
      .scanLeft(50): (acc, dial) =>
        dial match
          case s"R$d" => (acc + d.toInt) % 100
          case s"L$d" => (acc - d.toInt) % 100
      .count(_ == 0)

  def solve2(input: String): Int =
    val (_, count) = input.linesIterator
      .foldLeft(50 -> 0):
        case ((acc, cnt), s"R$d") =>
          val nextAcc = acc + d.toInt
          val nextCnt = (nextAcc.toDouble / 100).floor.toInt - (acc.toDouble / 100).floor.toInt
          (nextAcc, cnt + nextCnt)
        case ((acc, cnt), s"L$d") =>
          val nextAcc = acc - d.toInt
          val nextCnt = ((acc - 1).toDouble / 100).floor.toInt - ((nextAcc - 1).toDouble / 100).floor.toInt
          (nextAcc, cnt + nextCnt)
    count
