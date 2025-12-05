package aoc
package y2025

object Day5 extends Input(2025, 5):
  def solve(input: String): Int =
    val rangeStrings :: ids :: Nil = input.split(System.lineSeparator() * 2).toList

    val ranges = rangeStrings.linesIterator
      .map:
        case s"$from-$to" => from.toLong -> to.toLong
      .toVector

    ids.linesIterator.map(_.toLong).count(id => ranges.exists((b, e) => b <= id && id <= e))

  def solve2(input: String): Long =
    input.linesIterator
      .takeWhile(_.nonEmpty)
      .map:
        case s"$from-$to" => from.toLong -> to.toLong
      .toVector
      .sortBy(_(0))
      .foldLeft(Nil: List[(Long, Long)]):
        case (Nil, (b, e))                         => List((b, e))
        case (head @ ((b1, e1) :: tail), (b2, e2)) =>
          if (b1 >= b2 && b1 <= e2)
            || (e1 <= e2 && e1 >= b2)
            || (b1 >= b2 && e1 <= e2)
            || (b2 >= b1 && e2 <= e1)
            || e1 == b2 - 1
            || e2 == b1 - 1
          then (b1 min b2, e1 max e2) :: tail
          else (b2, e2) :: (b1, e1) :: tail
      .map((b, e) => e + 1 - b)
      .sum
