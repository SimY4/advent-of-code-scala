package aoc
package y2019

object Day10:
  private def asteroids(input: String): Vector[Coord] =
    (for
      (line, y) <- input.linesIterator.zipWithIndex
      x         <- line.indices
      if line.charAt(x) == '#'
    yield Coord(x, y)).toVector

  extension (c: Coord)
    private def angle(to: Coord): Double =
      (math.atan2((to.y - c.y).toDouble, (to.x - c.x).toDouble) + 2 * math.Pi + math.Pi / 2) % (2 * math.Pi)

    private def visibility(as: Vector[Coord]): Vector[Coord] =
      as
        .filter(_ != c)
        .groupMap: asteroid =>
          val dx    = c.x - asteroid.x
          val dy    = c.y - asteroid.y
          val ratio = dx.gcd(dy).abs
          Coord(dx / ratio, dy / ratio)
        (asteroid => asteroid -> c.distance(asteroid))
        .values
        .map(_.minBy(_(1))._1)
        .toVector
        .sortBy(c.angle)

  def solve(input: String): Long =
    val as = asteroids(input)
    as.map(_.visibility(as).size).max

  def solve2(input: String): Long =
    val as      = asteroids(input)
    val station = as.maxBy(_.visibility(as).size)

    val ths = LazyList
      .unfold(as): as =>
        val visible = station.visibility(as)
        Some(visible, as.filterNot(visible.contains))
      .flatten
      .drop(199)
      .head
    ths.x * 100 + ths.y

  val input = """#.#....#.#......#.....#......####.
                |#....#....##...#..#..##....#.##..#
                |#.#..#....#..#....##...###......##
                |...........##..##..##.####.#......
                |...##..##....##.#.....#.##....#..#
                |..##.....#..#.......#.#.........##
                |...###..##.###.#..................
                |.##...###.#.#.......#.#...##..#.#.
                |...#...##....#....##.#.....#...#.#
                |..##........#.#...#..#...##...##..
                |..#.##.......#..#......#.....##..#
                |....###..#..#...###...#.###...#.##
                |..#........#....#.....##.....#.#.#
                |...#....#.....#..#...###........#.
                |.##...#........#.#...#...##.......
                |.#....#.#.#.#.....#...........#...
                |.......###.##...#..#.#....#..##..#
                |#..#..###.#.......##....##.#..#...
                |..##...#.#.#........##..#..#.#..#.
                |.#.##..#.......#.#.#.........##.##
                |...#.#.....#.#....###.#.........#.
                |.#..#.##...#......#......#..##....
                |.##....#.#......##...#....#.##..#.
                |#..#..#..#...........#......##...#
                |#....##...#......#.###.#..#.#...#.
                |#......#.#.#.#....###..##.##...##.
                |......#.......#.#.#.#...#...##....
                |....##..#.....#.......#....#...#..
                |.#........#....#...#.#..#....#....
                |.#.##.##..##.#.#####..........##..
                |..####...##.#.....##.............#
                |....##......#.#..#....###....##...
                |......#..#.#####.#................
                |.#....#.#..#.###....##.......##.#.""".stripMargin
