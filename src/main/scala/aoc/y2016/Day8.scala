package aoc.y2016

object Day8:
  private type Grid = Array[Array[Boolean]]

  private enum Action:
    case Rect(x: Int, y: Int)
    case RotateRow(y: Int, shift: Int)
    case RotateCol(x: Int, shift: Int)

  import Action.*

  private def parseLine(line: String): Action = line match
    case s"rect ${x}x$y"                 => Rect(x.toInt, y.toInt)
    case s"rotate row y=$y by $shift"    => RotateRow(y.toInt, shift.toInt)
    case s"rotate column x=$x by $shift" => RotateCol(x.toInt, shift.toInt)

  private def decode(input: String): Grid = input.linesIterator
    .map(parseLine)
    .foldLeft(Array.fill(6, 50)(false)):
      case (grid, Rect(x, y)) =>
        for i <- 0 until y; j <- 0 until x do grid(i)(j) = true
        grid
      case (grid, RotateRow(y, shift)) =>
        val row    = grid(y)
        val newRow = Array.ofDim[Boolean](row.length)
        for i <- 0 until row.length do newRow((i + shift) % newRow.length) = row(i)
        grid(y) = newRow
        grid
      case (grid, RotateCol(x, shift)) =>
        val col = for i <- 0 until grid.length yield grid(i)(x)
        for i <- 0 until grid.length do grid((i + shift) % grid.length)(x) = col(i)
        grid

  def solve(input: String): Int = decode(input).map(_.count(identity)).sum

  def solve2(input: String): Unit = decode(input).foreach(arr => println(arr.map(if _ then 'X' else '.').mkString))

  val input = """rect 1x1
                |rotate row y=0 by 5
                |rect 1x1
                |rotate row y=0 by 6
                |rect 1x1
                |rotate row y=0 by 5
                |rect 1x1
                |rotate row y=0 by 2
                |rect 1x1
                |rotate row y=0 by 5
                |rect 2x1
                |rotate row y=0 by 2
                |rect 1x1
                |rotate row y=0 by 4
                |rect 1x1
                |rotate row y=0 by 3
                |rect 2x1
                |rotate row y=0 by 7
                |rect 3x1
                |rotate row y=0 by 3
                |rect 1x1
                |rotate row y=0 by 3
                |rect 1x2
                |rotate row y=1 by 13
                |rotate column x=0 by 1
                |rect 2x1
                |rotate row y=0 by 5
                |rotate column x=0 by 1
                |rect 3x1
                |rotate row y=0 by 18
                |rotate column x=13 by 1
                |rotate column x=7 by 2
                |rotate column x=2 by 3
                |rotate column x=0 by 1
                |rect 17x1
                |rotate row y=3 by 13
                |rotate row y=1 by 37
                |rotate row y=0 by 11
                |rotate column x=7 by 1
                |rotate column x=6 by 1
                |rotate column x=4 by 1
                |rotate column x=0 by 1
                |rect 10x1
                |rotate row y=2 by 37
                |rotate column x=19 by 2
                |rotate column x=9 by 2
                |rotate row y=3 by 5
                |rotate row y=2 by 1
                |rotate row y=1 by 4
                |rotate row y=0 by 4
                |rect 1x4
                |rotate column x=25 by 3
                |rotate row y=3 by 5
                |rotate row y=2 by 2
                |rotate row y=1 by 1
                |rotate row y=0 by 1
                |rect 1x5
                |rotate row y=2 by 10
                |rotate column x=39 by 1
                |rotate column x=35 by 1
                |rotate column x=29 by 1
                |rotate column x=19 by 1
                |rotate column x=7 by 2
                |rotate row y=4 by 22
                |rotate row y=3 by 5
                |rotate row y=1 by 21
                |rotate row y=0 by 10
                |rotate column x=2 by 2
                |rotate column x=0 by 2
                |rect 4x2
                |rotate column x=46 by 2
                |rotate column x=44 by 2
                |rotate column x=42 by 1
                |rotate column x=41 by 1
                |rotate column x=40 by 2
                |rotate column x=38 by 2
                |rotate column x=37 by 3
                |rotate column x=35 by 1
                |rotate column x=33 by 2
                |rotate column x=32 by 1
                |rotate column x=31 by 2
                |rotate column x=30 by 1
                |rotate column x=28 by 1
                |rotate column x=27 by 3
                |rotate column x=26 by 1
                |rotate column x=23 by 2
                |rotate column x=22 by 1
                |rotate column x=21 by 1
                |rotate column x=20 by 1
                |rotate column x=19 by 1
                |rotate column x=18 by 2
                |rotate column x=16 by 2
                |rotate column x=15 by 1
                |rotate column x=13 by 1
                |rotate column x=12 by 1
                |rotate column x=11 by 1
                |rotate column x=10 by 1
                |rotate column x=7 by 1
                |rotate column x=6 by 1
                |rotate column x=5 by 1
                |rotate column x=3 by 2
                |rotate column x=2 by 1
                |rotate column x=1 by 1
                |rotate column x=0 by 1
                |rect 49x1
                |rotate row y=2 by 34
                |rotate column x=44 by 1
                |rotate column x=40 by 2
                |rotate column x=39 by 1
                |rotate column x=35 by 4
                |rotate column x=34 by 1
                |rotate column x=30 by 4
                |rotate column x=29 by 1
                |rotate column x=24 by 1
                |rotate column x=15 by 4
                |rotate column x=14 by 1
                |rotate column x=13 by 3
                |rotate column x=10 by 4
                |rotate column x=9 by 1
                |rotate column x=5 by 4
                |rotate column x=4 by 3
                |rotate row y=5 by 20
                |rotate row y=4 by 20
                |rotate row y=3 by 48
                |rotate row y=2 by 20
                |rotate row y=1 by 41
                |rotate column x=47 by 5
                |rotate column x=46 by 5
                |rotate column x=45 by 4
                |rotate column x=43 by 5
                |rotate column x=41 by 5
                |rotate column x=33 by 1
                |rotate column x=32 by 3
                |rotate column x=23 by 5
                |rotate column x=22 by 1
                |rotate column x=21 by 2
                |rotate column x=18 by 2
                |rotate column x=17 by 3
                |rotate column x=16 by 2
                |rotate column x=13 by 5
                |rotate column x=12 by 5
                |rotate column x=11 by 5
                |rotate column x=3 by 5
                |rotate column x=2 by 5
                |rotate column x=1 by 5""".stripMargin
