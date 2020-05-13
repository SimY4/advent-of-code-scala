package aoc
package y2017

object Day3 {
  private inline def sqr(i: Int) = i * i

  def solve(input: Int): Int = {
    val diag = LazyList.from(1).dropWhile { i => sqr(i + i + 1) < input }.head
    val from = sqr((diag - 1) * 2 + 1) + 1
    val to = sqr(diag * 2 + 1)
    val side = (to + 1 - from) / 4
    val pos = (input - from) % side
    val center = (side / 2) - 1

    if (pos >= center) pos - center + diag
    else center - pos + diag
  }

  def solve2(input: Int): Option[Int] = {
    def indexes(x: Long): Seq[Coord] = x match {
      case 0L => Seq(Coord(0, 0))
      case n => (-n + 1 to n).map(Coord(n, _)) ++
        ((n - 1) to (-n, -1)).map(Coord(_, n)) ++
        ((n - 1) to (-n, -1)).map(Coord(-n, _)) ++
        ((-n + 1) to n).map(Coord(_, -n))
    }

    val coords = LazyList.iterate(0L)(_ + 1L).flatMap(indexes)
    coords.tail
      .scanLeft(Map(Coord(0L, 0L) -> 1)) { (acc, coord) => 
        val res = (for {
          neighbour <- coord.neighbours
          v = acc.getOrElse(neighbour, 0)
        } yield v).sum
        acc + (coord -> res)
      }
      .map(_.values.max)
      .find(_ > input)
  }

  val input = 347991
}