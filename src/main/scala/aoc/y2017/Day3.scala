package aoc
package y2017

import scala.annotation.tailrec

object Day3 {

  private def sqr(i: Int) = i * i

  def distance(x: Int): Int = {
    val diag = Stream
      .from(1)
      .dropWhile { i =>
        sqr(i + i + 1) < x
      }
      .head
    val from   = sqr((diag - 1) * 2 + 1) + 1
    val to     = sqr(diag * 2 + 1)
    val side   = (to + 1 - from) / 4
    val pos    = (x - from) % side
    val center = (side / 2) - 1

    if (pos >= center)
      pos - center + diag
    else
      center - pos + diag
  }

  println(distance(1) == 0)
  println(distance(12) == 3)
  println(distance(23) == 2)
  println(distance(1024) == 31)

  // PART 2

  def firstLarger(X: Int): Int = {
    type Coord = (Int, Int)

    def indexes(x: Int): Seq[Coord] = x match {
      case 0 => Seq(0 -> 0)
      case n =>
        (-n + 1 to n).map(n         -> _) ++
          (n - 1).to(-n, -1).map(_  -> n) ++
          (n - 1).to(-n, -1).map(-n -> _) ++
          (-n + 1 to n).map(_       -> -n)
    }

    def neighbours(coord: Coord): Seq[Coord] = coord match {
      case (x, y) =>
        Seq(
          x - 1 -> (y - 1),
          x     -> (y - 1),
          x + 1 -> (y - 1),
          x - 1 -> y,
          x + 1 -> y,
          x - 1 -> (y + 1),
          x     -> (y + 1),
          x + 1 -> (y + 1)
        )
    }

    @tailrec def firstLarger0(accSeq: Map[Coord, Int], seqGen: Stream[Coord]): Int = {
      val coord = seqGen.head

      val res = (for {
        neighbour <- neighbours(coord)
        v         = accSeq.getOrElse(neighbour, 0)
      } yield v).sum

      if (res > X)
        res
      else
        firstLarger0(accSeq ++ Map(coord -> res), seqGen.tail)
    }

    def coords = Stream.from(0).flatMap(indexes)

    firstLarger0(Map(0 -> 0 -> 1), coords.tail)
  }

  println(firstLarger(747))

}
