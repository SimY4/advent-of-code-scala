package AdventOfCode
package y2018

import scala.annotation.tailrec

object Day6 {

  val input = """1, 1
                |1, 6
                |8, 3
                |3, 4
                |5, 5
                |8, 9""".stripMargin

  def coords(input: String): List[(Int, Int)] =
    input.linesIterator.map { line =>
      line.split(", ").map(_.toInt).toList match {
        case x :: y :: Nil => x -> y
      }
    }.toList

  def dist(from: (Int, Int), to: (Int, Int)): Int =
    math.abs(to._1 - from._1) + math.abs(to._2 - from._2)

  def solve(input: String): Int = {
    val coord = coords(input)

    def closest(c: (Int, Int)): String = {
      val dists = coord.map(dist(_, c))
      val minDist = dists.min
      val indexes = dists.zipWithIndex.filter(_._1 == minDist)
      if (indexes.size > 1) "."
      else {
        val idx = indexes.head._2
        val letter = ('a'.toInt + idx).toChar
        s"$letter$idx"
      }
    }

    val minX = coord.map(_._1).min
    val maxX = coord.map(_._1).max
    val minY = coord.map(_._2).min
    val maxY = coord.map(_._2).max

    type Rect = Seq[((Int, Int), String)]

    def boundaries(rect: Rect): Set[String] =
      (for {
        ((x, y), letter) <- rect
        if x == minX || x == maxX || y == minY || y == maxY
      } yield letter).toSet

    def frequencies(rect: Rect): Map[String, Int] =
      rect.map(_._2).groupBy(identity).mapValues(_.size)

    val rect = for {
      x <- minX to maxX
      y <- minY to maxY
    } yield (x -> y) -> closest(x -> y)

    val bndries = boundaries(rect)
    val freqs = frequencies(rect)
    val safe = freqs -- bndries

    safe.map(_._2).max
  }

  def solve2(input: String): Int = {
    val coord = coords(input)

    val minX = coord.map(_._1).min
    val maxX = coord.map(_._1).max
    val minY = coord.map(_._2).min
    val maxY = coord.map(_._2).max

    val rect = for {
      x <- minX to maxX
      y <- minY to maxY
      d = coord.map(dist(_, x -> y)).reduce(_ + _)
      if d < 10000
    } yield x -> y

    rect.size
  }
}
