package aoc
package y2017

import scala.annotation.tailrec

object Day3 {
  private def sqr(i: Int) = i * i

  def solve(x: Int): Int = {
    val diag = LazyList.from(1).dropWhile { i => sqr(i + i + 1) < x }.head
    val from = sqr((diag - 1) * 2 + 1) + 1
    val to = sqr(diag * 2 + 1)
    val side = (to + 1 - from) / 4
    val pos = (x - from) % side
    val center = (side / 2) - 1

    if (pos >= center) pos - center + diag
    else center - pos + diag
  }

  def solve2(X: Int): Int = {
    def (coord: Coord) neighbours: Seq[Coord] = {
      val Coord(x, y) = coord
      Seq(
        Coord(x - 1, y - 1),
        Coord(x, y - 1),
        Coord(x + 1, y - 1),
        Coord(x - 1, y),
        Coord(x + 1, y),
        Coord(x - 1, y + 1),
        Coord(x, y + 1),
        Coord(x + 1, y + 1)
      )
    }

    def indexes(x: Long): Seq[Coord] = x match {
      case 0L => Seq(Coord(0, 0))
      case n => (-n + 1 to n).map(Coord(n, _)) ++
        (n - 1).to(-n, -1).map(Coord(_, n)) ++
        (n - 1).to(-n, -1).map(Coord(-n, _)) ++
        (-n + 1 to n).map(Coord(_, -n))
    }

    @tailrec def firstLarger0(accSeq: Map[Coord, Int], seqGen: LazyList[Coord]): Int = {
      val coord = seqGen.head

      val res = (for {
        neighbour <- coord.neighbours
        v = accSeq.getOrElse(neighbour, 0)
      } yield v).sum

      if (res > X) res
      else firstLarger0(accSeq ++ Map(coord -> res), seqGen.tail)
    }

    def coords = LazyList.iterate(0L)(_ + 1).flatMap(indexes)
    firstLarger0(Map(Coord(0, 0) -> 1), coords.tail)
  }
}