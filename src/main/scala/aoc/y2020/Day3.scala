package aoc
package y2020

import scala.annotation.tailrec

object Day3:
  def solve(input: String): Int =
    val area = input.linesIterator.map { line =>
      def list: LazyList[Char] = LazyList(line.toList*) #::: list
      list
    }.toList

    @tailrec def loop(coord: Coord, acc: Int = 0): Int =
      area.lift((coord.y + 1L).toInt) match
        case None => acc
        case Some(axis) =>
          loop(Coord(x = coord.x + 3L, y = coord.y + 1L), if axis((coord.x + 3L).toInt) == '#' then acc + 1 else acc)

    loop(Coord(0L, 0L))

  def solve2(input: String): Long =
    val area = input.linesIterator.map { line =>
      def list: LazyList[Char] = LazyList(line.toList*) #::: list
      list
    }.toList

    @tailrec def loop(coord: Coord, slope: Coord, acc: Int = 0): Long =
      area.lift((coord.y + slope.y).toInt) match
        case None => acc.toLong
        case Some(axis) =>
          loop(
            Coord(x = coord.x + slope.x, y = coord.y + slope.y),
            slope,
            if axis((coord.x + slope.x).toInt) == '#' then acc + 1 else acc
          )

    List(Coord(1L, 1L), Coord(3L, 1L), Coord(5L, 1L), Coord(7L, 1L), Coord(1L, 2L))
      .map(loop(Coord(0L, 0L), _))
      .product

  val input = """......##....#...#..#.#....#....
                |.......#...#..#..#....##.......
                |#.#...#........###.#.##..#.....
                |.......#.....##.#..##...##.##..
                |.#...#.#...##..........#..#....
                |#.......##.....###.#...#......#
                |.........#....#.#.......#..#...
                |..#.......####.......###..##...
                |.#.#.##..#.#...##..#...###...##
                |...................#...........
                |....#.#.......#..........#.#..#
                |..#.#...........####.#.......#.
                |.....#.##..#..##..#.#...#......
                |#.##...###..#................##
                |...#...#...#..##.#............#
                |#.##....##....#........#..#....
                |..#......#.#.....##.......#....
                |.......#......#....#......#....
                |.#........##.....#.#...#...#.#.
                |..........##.#...#..#..........
                |#####..##......#.....#......#.#
                |......#...............##...#...
                |..#.#.##..#...#.#........#...#.
                |..........#......#..........###
                |..#...##.##..##..........#.....
                |........#.##.#.....#..#...#....
                |#.....#.........#..............
                |..........##.##....#..#..#.....
                |..#...........#.......#........
                |........#..#.....#.#.#...#.....
                |#.......##.....#.....#...#.##..
                |###.#.#....#..#.....#........#.
                |..#..#..#..........#....#....#.
                |..#...##...#.#.##.....#..#.....
                |...#....###...........##.#.....
                |.##.................##.#.......
                |........#...#.##..#...#........
                |.##..#............##..........#
                |............###.#....#..#......
                |.....##....#.....#......#.....#
                |....#.....#.##.......#...#.#...
                |.##.#......#.........#...##....
                |..##......#......#...........#.
                |.......#.#.............#.......
                |.##.#...#..##....##.......#....
                |...#......##.#.#......#.....###
                |#.#....#.......#.#......#....#.
                |#......#.#.....#...........#..#
                |##.#..##...#........#.##.#....#
                |.....#........#........#...#...
                |...............#.......#..#....
                |.#.#.#..#.#...#.......#.....##.
                |.#.#.............#..#....#.....
                |....#.......#..##.........###..
                |.#.....#.#....#..#..#....#.....
                |........#......#.....#.#....#..
                |##......#....##.....#.#..#.#...
                |.#...#..#.##.#.##.##.....#.....
                |#...#....#.........##.#....#...
                |.........##..#.....#..#...#.#..
                |.#............#..........#.#...
                |...........#.....#......#.#....
                |#...#...#.....#..#....#........
                |#..##.....#..#.......#....#...#
                |#..#..#..........#......#...#..
                |...#...#.#.##.#...#....#...##..
                |......##....##....#....##..####
                |...###.#..#....#.......#.......
                |#.........##......#...#........
                |..........#....#.......#.......
                |#....##................##....##
                |.........#....#.#.......##.#...
                |.....#......###.......#..#...##
                |###.....#..##....###...........
                |.....#...#....#.....##......###
                |.#..#...#......##........##..#.
                |#.#.#.#....#.............#.....
                |......#.....##.#....#..##...#..
                |..#............#.#....#..#...#.
                |.............#.#...##.......#..
                |...#....#.##.#...#.#..##...###.
                |...#..............#.......#....
                |......###.#............#.....#.
                |.##...###..#.####...#..........
                |...#..#...#.#.#..#......#..#...
                |.#....##.###....#........#.....
                |..#..#....#.........##.........
                |..........##.###........#.#...#
                |.........#...#..#........#.....
                |.......#.....#...###...........
                |.....#.#..##......#...#...#....
                |.....#....#..#........##.#..#..
                |...#...........#............#..
                |##.....#....#.#...#...#....##..
                |...#.....#.....#...##...#...#..
                |...##.#..........##...#.#.##.#.
                |....#.#.##.......#.#...#......#
                |......###...#....#.##........#.
                |.....#.........#...#...#..#..##
                |.........#................#....
                |.##..###..................#.#.#
                |.##...........#...........#....
                |#...#........#.....#..#...##...
                |.....#..#...#.........#.......#
                |..#..............#......#......
                |#....#...............#.#.......
                |...#........#.#....#..#.###.##.
                |.......#..##..#...#..#...###...
                |..........##..#.......##.##....
                |##.#..#.#...##..........#......
                |.#.##.#...##.....#....#....#.##
                |...#.#......#...#.##..##.......
                |##.......#.#......#....##..#.#.
                |...#..#.##.........#...#.....#.
                |.##.##..##...#........#..#.....
                |.#.##.............#.#.#.....#..
                |.......#.....................#.
                |......#...#....#..#..........#.
                |..#..#....#.#................#.
                |..#.....#..#.#......#......###.
                |...#...##..##....#..#...###.#..
                |...#.....#............##......#
                |.......#.#.#......#.....###....
                |.....#......#.....#.........#.#
                |#...#.#...#..#...#..#....#.....
                |#..##...#..##.............#..#.
                |##....##.......#.#.......#..#.#
                |..............#...#..#......#..
                |..#...#...#.#...#.#............
                |#..........#...#.............#.
                |..........##......#........#...
                |#...#...#....#.#...........#...
                |..#.#.#...##......#.#...#.#..#.
                |.......#.......#.............#.
                |.#..........#..................
                |..##...#......#..........#....#
                |.#..##..........#...#..........
                |...#....#..#.#.....##..##.#..#.
                |...#...#...#..#....##..#....#..
                |..............#.#.....#......##
                |..............####....#.#..#...
                |.#........##....#...#.#...#..#.
                |.#..##.###....#.#.....##..#....
                |...###.#.........#..#..#.##.#..
                |.....#..#.....#..#...##......##
                |.#.#.##.............#...##.....
                |....##........#........#.......
                |.......#.....###..............#
                |#.##.......##....#.#.....#.#...
                |........#....#............#..##
                |...#.#..#.......#..........#...
                |..##....#..##......###.#.....#.
                |.#..#.#.##....#.......#........
                |........#.####.#.......#.##....
                |..........##...............#...
                |.#..#.....#....##..#..##...#..#
                |....#.#.....#.#.........#####..
                |...#.##....#...###.##.#..#.....
                |.#...........#.............##.#
                |..#....#....####.....#.#....#..
                |......##.......#....#..#.......
                |.####...##.#.#..#.####.#.#.....
                |###.........#..#.#.#.#........#
                |...#...#..#.............#.##...
                |.........#....#......#.....#.#.
                |...#....#......#..#......#....#
                |..#...#..........##..##........
                |.....##........#......#.....#..
                |...#....#....#....#..#....#....
                |##...#...........##............
                |.......#..##..#.......##.#.....
                |...............#.##.....#......
                |#.#....##.#.....#...#..........
                |........#......#...#......#.#.#
                |..#..#.....#.#........#........
                |..####.....##.#.##.......#.#.#.
                |.#.##.#.......##......#.....#..
                |....#.....##.........#.....#...
                |.#.#...###.#.#..........#....#.
                |.........##.#.#.....#..#.......
                |......#..#...#..#..###.#.#.....
                |.....#...#.#..#.#.......#.#...#
                |......##........#..#...#......#
                |#..##...#...#..#.....#..#..#..#
                |......#....#...........#.#.....
                |...#.......#...............#...
                |#.........#......#.............
                |..###..................#......#
                |#.....#.#.#.......##....#......
                |.........#...........#....#.#..
                |.###....##.##..##.............#
                |.##.#......#...#...##..........
                |....#........###......#.#......
                |...........#..#.##.#...........
                |.#..#.......#......#.#####.....
                |....##....##......#....#...#...
                |.......#..#.....#.#...###...#.#
                |..##.....#.......#.#.#..#.....#
                |.#...#............#....##...#..
                |.#..#...##.......#.............
                |..##.......#...........#.#....#
                |...#.#...#....#..#.....#.......
                |...#........#...##...#.#..#.#..
                |#........#..........#..........
                |......#......#.........#.......
                |...##...#.....#.......#...#.##.
                |......##..##......#..###..#....
                |....##....#..###.#.....##......
                |##.##..#.....#..#..............
                |..#.#..#....#....#....#.#...#.#
                |.#.....##.#.##.#..#.#..........
                |...#......##.#...##..##...#....
                |.###.....#......#.......#.....#
                |....##.......#.....#..#....#...
                |..........#..##....#..##.#....#
                |...#....#..##.#........#.#.#...
                |...#.#...#....#.......#..##.#.#
                |#..#..........#.#...#....#.....
                |#..#...........................
                |........#.....#.....#.#...#.#..
                |#...#..#...#..........###...#.#
                |.....##.#..##.#.#.#.##....#....
                |#.......#....#.#..#..#..#.#....
                |..###.#.......#.#.##...........
                |#....#..#..........#.##..#.#...
                |..#..#........##....#..##......
                |#...##..#.........#.#....#.#...
                |##..###..##...#.........#.#...#
                |###..#....#..##...#.#..#.#.....
                |.#.##.#......#............#....
                |.#...#.##.#.........##.........
                |##.....###.....#........#..#...
                |...........##.#................
                |.#......###......#....#..####..
                |#...##.....#.....#..##....#.#..
                |..#....#.......#.#.#......#...#
                |#.....#........#....#.#...#....
                |..##...............#....#..###.
                |.#....#.......#..#...#.........
                |.##.#..#..#...#..#..#....#....#
                |.......#.#....#.....##...#.....
                |.#....#.#.#...........#........
                |.........#..##..#..#...#.......
                |##..##...#......#.....#........
                |#...........#.....#..###......#
                |.#...........#....#...#...##.#.
                |..............##.###.#.#####.##
                |........#.#...#.............##.
                |#...................###..#.##..
                |#.....#...##...................
                |.....##..........#..#.#........
                |.#....##.#....#....###....#...#
                |.......#.#...........#.#.....#.
                |......#........###...#...##....
                |.##..........#..#..#...........
                |....#.......#..#.....##.#..#...
                |..#.##......#..#.....#..#......
                |......#...#..##....#.#..#..#.#.
                |#.........................#...#
                |###.#.......#......##....#..#..
                |..##.###.#...#.............#...
                |.....#...#...#......#....#####.
                |#..........#.#.##.#.#.....#..#.
                |....#.........#...#.#.........#
                |#.##.........#...#...#.####..##
                |.##.................#..........
                |##.....#............#..#.#.....
                |#.#...#.#........#........#...#
                |.#...........#....#..#.......#.
                |.#.......#..........##..#.##..#
                |.#..##....#..##......#.#..##...
                |#......#............#.......#.#
                |.##...............#...#...#....
                |.......##.#..#..##.....#.......
                |...#.......#..###.....#....#...
                |......#............#...........
                |####............#.........#.##.
                |#......#.#..#...#.....#..#.....
                |...........#...#..##.......####
                |#.#...##..#....#.#.........#.#.
                |...#....#..#.......#.........#.
                |.........#.#.#...#....#........
                |.#.....#........#..#.........#.
                |....#....#..#.....#...#........
                |..#....#.#.....#..##...........
                |.#...#..#..#.##.###....#.......
                |#......##.......##..##.........
                |...#.........#.......##.#......
                |.#...#...#.......#........##...
                |..#.............#.......#.....#
                |..#...........#.#.#...#.......#
                |.....##..#....#..............#.
                |#.#.....#.#....................
                |.....#..##..#...#.....#........
                |..#.......#..####..#....#.##.#.
                |#....#.....#.....#...#......#..
                |..#....##...#....#..#..#.....#.
                |..#.####..............##.......
                |.#.........#..#...#.......##...
                |#....#.#........#....#...#...##
                |.....#..#....#.#..#...#.#.##...
                |.##.................#...##.....
                |.##.##.##...#...........#...##.
                |..#....#..#.....#..#......##...
                |.#...........#......#....#..#.#
                |.#.#............#..#..#...#....
                |....#......#.....#.#.#.....#...
                |#.......##.............#.......
                |....#....................#.#...
                |......#........#..#.#.....#.#..
                |.....#..#....#.#........#....#.
                |...##.........#...#.##....#..#.
                |.#....#..#...#.#.#......#......
                |#......#.#.##.#..#..#.....##...
                |......#....#.#...#..#.#........
                |..#.....##.....#...#.#.......#.
                |......#.#.....#........#.......
                |......#.#.#...#..#.#.#.#.......
                |..#.#.##..#..#..#.#.##...#.....
                |......#.#.#......#.....#...#...
                |.....#.##....#..##...#...#....#
                |..#.....#...........#..#..##...
                |..#..#.......#....#....###.#...""".stripMargin
