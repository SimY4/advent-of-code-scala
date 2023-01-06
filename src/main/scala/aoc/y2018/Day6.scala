package aoc
package y2018

object Day6:
  private val linePattern = "(\\d+), (\\d+)".r
  def coords(input: String): List[Coord] =
    input.linesIterator.collect { case linePattern(x, y) => Coord(x.toLong, y.toLong) }.toList

  def solve(input: String): Int =
    val coord = coords(input)

    def closest(c: Coord): String =
      val dists   = coord.map(_.manhattan(c))
      val minDist = dists.min
      val indexes = dists.zipWithIndex.filter(_._1 == minDist)
      if indexes.size > 1 then "."
      else
        val idx    = indexes.head._2
        val letter = ('a'.toInt + idx).toChar
        s"$letter$idx"

    val minX = coord.map(_.x).min
    val maxX = coord.map(_.x).max
    val minY = coord.map(_.y).min
    val maxY = coord.map(_.y).max

    type Rect = Seq[(Coord, String)]

    def boundaries(rect: Rect): Set[String] =
      (for
        (Coord(x, y), letter) <- rect
        if x == minX || x == maxX || y == minY || y == maxY
      yield letter).toSet

    def frequencies(rect: Rect): Map[String, Int] =
      rect.map(_._2).groupBy(identity).view.mapValues(_.size).toMap

    val rect = for
      x <- minX to maxX
      y <- minY to maxY
      coords = Coord(x, y)
    yield coords -> closest(coords)

    val bndries = boundaries(rect)
    val freqs   = frequencies(rect)
    val safe    = freqs -- bndries

    safe.map(_._2).max

  def solve2(input: String): Int =
    val coord = coords(input)

    val minX = coord.map(_.x).min
    val maxX = coord.map(_.x).max
    val minY = coord.map(_.y).min
    val maxY = coord.map(_.y).max

    val rect = for
      x <- minX to maxX
      y <- minY to maxY
      c = Coord(x, y)
      d = coord.map(_.manhattan(c)).reduce(_ + _)
      if d < 10000
    yield c

    rect.size

  val input = """315, 342
                |59, 106
                |44, 207
                |52, 81
                |139, 207
                |93, 135
                |152, 187
                |271, 47
                |223, 342
                |50, 255
                |332, 68
                |322, 64
                |250, 72
                |165, 209
                |129, 350
                |139, 118
                |282, 129
                |311, 264
                |216, 246
                |134, 42
                |66, 151
                |263, 199
                |222, 169
                |236, 212
                |320, 178
                |202, 288
                |273, 190
                |83, 153
                |88, 156
                |284, 305
                |131, 90
                |152, 88
                |358, 346
                |272, 248
                |317, 122
                |166, 179
                |301, 307
                |156, 128
                |261, 290
                |268, 312
                |89, 53
                |324, 173
                |353, 177
                |91, 69
                |303, 164
                |40, 221
                |146, 344
                |61, 314
                |319, 224
                |98, 143""".stripMargin
