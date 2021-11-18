package aoc
package y2017

object Day10 {
  final case class State(list: List[Int], lengths: List[Int], count: Int, pos: Int)

  private val lengths = "3,4,1,5".split(",").map(_.toInt).toList

  def list(n: Int): List[Int] = LazyList.from(0).take(n).toList

  def knotHash(state: State): State = {
    val State(list, lengths, count, pos) = state
    val (nextList, nextCount, nextPos) = lengths.foldLeft((list, count, pos)) { case ((ls, c, p), length) =>
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
        .map { i =>
          reversed.getOrElse(i, ls(i))
        }
        .toList
      (updated, c + 1, (p + length + c) % ls.size)
    }

    State(nextList, lengths, nextCount, nextPos)
  }

  val state: State                      = State(list(5), lengths, 0, 0)
  val State(l @ x1 :: x2 :: _, _, _, _) = knotHash(state): @unchecked
  println(l)
  println(x1 * x2 == 12)

  // PART 2

  private val rest = Seq(17, 31, 73, 47, 23)

  private val lengthsList = for
    (input, result) <- Map(
      ""         -> "a2582a3a0e66e6e86e3812dcb672a272",
      "AoC 2017" -> "33efeb34ea91902bb2f59c9920caa6cd",
      "1,2,3"    -> "3efbe78a8d82f29979031a4aa0b16a9d",
      "1,2,4"    -> "63960835bcdc130f0b66d7ff4f6a5a8e"
    )
    ascii = input.map(_.toInt).toList ++ rest
  yield ascii -> result

  def denseHash(state: State): String = {
    val State(list, _, _, _) = LazyList.from(0).take(64).foldLeft(state) { (acc, _) =>
      knotHash(acc)
    }
    (for
      ls <- list.sliding(16, 16).toList
      hex = s"0${ls.reduce(_ ^ _).toHexString}".takeRight(2)
    yield hex).mkString("")
  }

  for
    (i, r) <- lengthsList
    state: State = State(list(256), i, 0, 0)
    res          = denseHash(state)
    _            = println(s"$res -> $r")
    _            = println(res == r)
  yield res

}
