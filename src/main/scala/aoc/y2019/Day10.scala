package aoc.y2019

object Day10 {
  def solve(input: String): Int = {
    val layer = input.toSeq.sliding(25 * 6, 25 * 6)
      .minBy(_.filter('0' == _).length)
    layer.filter('1' == _).length * layer.filter('2' == _).length
  }
    
  def solve2(input: String): Unit = {
    val layers = input.toList.sliding(25 * 6, 25 * 6).map(_.mkString)

    println((for 
      pixel <- 0 until (25 * 6)
      color  = layers
        .map(_.charAt(pixel))
        .find(_ != '2')
        .getOrElse('0')
    yield color)
      .sliding(25, 25)
      .map { 
        _.map {
          case '0' => " "
          case '1' => "X"
          case _ => "_"
        }.mkString
      }
      .mkString("\n"))
  }

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
}