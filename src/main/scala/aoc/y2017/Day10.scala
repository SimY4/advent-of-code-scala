package aoc
package y2017

object Day10:
  private case class State(list: List[Int], lengths: List[Int], count: Int = 0, pos: Int = 0)

  private def knotHash(state: State): State =
    val State(list, lengths, count, pos) = state
    val (nextList, nextCount, nextPos)   = lengths.foldLeft((list, count, pos)):
      case ((ls, c, p), length) =>
        val reversed = Iterator
          .continually(ls)
          .flatten
          .slice(p, p + length)
          .toList
          .reverse
          .zipWithIndex
          .map((a, i) => ((p + i) % ls.size, a))
          .toMap
        val updated = LazyList
          .from(0)
          .take(ls.size)
          .map: i =>
            reversed.getOrElse(i, ls(i))
          .toList
        (updated, c + 1, (p + length + c) % ls.size)

    State(nextList, lengths, nextCount, nextPos)

  def solve(input: String, n: Int = 256): Int =
    val lengths = input.split(",").map(_.toInt).toList
    knotHash(State(List.range(0, n), lengths)).list.take(2).sum

  def solve2(input: String, encoding: Int => String = i => f"$i%02x"): String =
    val list = LazyList
      .iterate(State(List.range(0, 256), input.map(_.toInt).toList ::: List(17, 31, 73, 47, 23)))(knotHash)
      .drop(64)
      .head
      .list
    (for ls <- list.grouped(16).toList
    yield encoding(ls.reduce(_ ^ _))).mkString

  val input = "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"
