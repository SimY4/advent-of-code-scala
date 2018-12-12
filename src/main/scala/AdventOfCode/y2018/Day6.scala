package AdventOfCode
package y2018

object Day6 {

  val input = """1, 1
                |1, 6
                |8, 3
                |3, 4
                |5, 5
                |8, 9""".stripMargin

  case class Coords(x: Int, y: Int)
  implicit class CoordsOps(private val coords: Coords) extends AnyVal {
    def dist(to: Coords): Int =
      math.abs(to.x - coords.x) + math.abs(to.y - coords.y)
  }

  def coords(input: String): List[Coords] =
    input.linesIterator.map { line =>
      line.split(", ").map(_.toInt).toList match {
        case x :: y :: Nil => Coords(x, y)
      }
    }.toList

  def solve(input: String): Int = {
    val coord = coords(input)

    def closest(c: Coords): String = {
      val dists = coord.map(_.dist(c))
      val minDist = dists.min
      val indexes = dists.zipWithIndex.filter(_._1 == minDist)
      if (indexes.size > 1) "."
      else {
        val idx = indexes.head._2
        val letter = ('a'.toInt + idx).toChar
        s"$letter$idx"
      }
    }

    val minX = coord.map(_.x).min
    val maxX = coord.map(_.x).max
    val minY = coord.map(_.y).min
    val maxY = coord.map(_.y).max

    type Rect = Seq[(Coords, String)]

    def boundaries(rect: Rect): Set[String] =
      (for {
        (Coords(x, y), letter) <- rect
        if x == minX || x == maxX || y == minY || y == maxY
      } yield letter).toSet

    def frequencies(rect: Rect): Map[String, Int] =
      rect.map(_._2).groupBy(identity).mapValues(_.size)

    val rect = for {
      x <- minX to maxX
      y <- minY to maxY
      coords = Coords(x, y)
    } yield coords -> closest(coords)

    val bndries = boundaries(rect)
    val freqs = frequencies(rect)
    val safe = freqs -- bndries

    safe.map(_._2).max
  }

  def solve2(input: String): Int = {
    val coord = coords(input)

    val minX = coord.map(_.x).min
    val maxX = coord.map(_.x).max
    val minY = coord.map(_.y).min
    val maxY = coord.map(_.y).max

    val rect = for {
      x <- minX to maxX
      y <- minY to maxY
      c = Coords(x, y)
      d = coord.map(_.dist(c)).reduce(_ + _)
      if d < 10000
    } yield c

    rect.size
  }
}
