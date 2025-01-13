package aoc
package y2024

object Day8:
  def solve(input: String): Int =
    val (maxX, maxY) = (input.linesIterator.size, input.linesIterator.size)
    val signals = (for
      (line, y) <- input.linesIterator.zipWithIndex
      (char, x) <- line.zipWithIndex
      if Character.isLetterOrDigit(char)
    yield char -> Coord(x, y)).toVector.groupMap(_(0))(_(1))

    (for
      coords <- signals.values
      c1     <- coords
      c2     <- coords
      if c1 != c2
      antinode = Coord(c1.x - (c2.x - c1.x), c1.y - (c2.y - c1.y))
      if 0 <= antinode.x && 0 <= antinode.y && antinode.x < maxX && antinode.y < maxY
    yield antinode).toSet.size

  def solve2(input: String): Int =
    val (maxX, maxY) = (input.linesIterator.size, input.linesIterator.size)
    val signals = (for
      (line, y) <- input.linesIterator.zipWithIndex
      (char, x) <- line.zipWithIndex
      if Character.isLetterOrDigit(char)
    yield char -> Coord(x, y)).toVector.groupMap(_(0))(_(1))

    (for
      coords <- signals.values
      c1     <- coords
      c2     <- coords
      if c1 != c2
      (dx, dy) = (c2.x - c1.x, c2.y - c1.y)
      antinode <- Iterator
        .iterate(c1)(c => Coord(c.x - dx, c.y - dy))
        .takeWhile(a => 0 <= a.x && 0 <= a.y && a.x < maxX && a.y < maxY)
    yield antinode).toSet.size

  val input = """...............................s..................
                |..................s..............q.............p..
                |.....a............................................
                |........c......Y.......Q..........................
                |............................................4.....
                |........Y.........y............m..........4.......
                |......................Y...s..........S............
                |.........................................S........
                |...............N.............y....................
                |...........a.......y..................1...........
                |................................................S.
                |...c........k.............q....t............S.....
                |.............................qM...................
                |........a.........................................
                |..................................................
                |..................................................
                |..c..........k...Q..q....P........................
                |5.................Q...................8...........
                |......yc..........................................
                |........................E............4............
                |.........6........................u..p.....4......
                |.........5.............P..n......1.........N......
                |6..............................1.........J.t......
                |..6..................................3.u..t.....p.
                |....5...k..........................u..............
                |.......................E..................u....x..
                |..................E.................x.............
                |...k..................P.............3.............
                |...........0.....9.5...........E.........31e....N.
                |......0.................................N.........
                |.................CU.....................t....x....
                |......7....................e......................
                |....0..........K......C...........................
                |.....6....j......M............................J...
                |......K.................................p.........
                |.....9........................U...................
                |............................3....n................
                |.............K.........2.....C..................x.
                |....................P........UJ...................
                |.....0......X...C.........T..............U........
                |.......M.....8j....7.............2........Q.......
                |9...............K.................................
                |....e.....8.........................2.A.m.........
                |..e......8.........s...n..........................
                |.....................................T..nm........
                |...................X............2.........m......A
                |......................X..j....................T...
                |.........7..M......j.............T................
                |....9...7....................................A....
                |..........................................A.......""".stripMargin
