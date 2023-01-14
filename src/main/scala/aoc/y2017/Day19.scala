package aoc
package y2017

object Day19 extends Input(2017, 19):
  private def parseInput(input: String): LazyList[Char] =
    val maze  = input.linesIterator.toVector
    val start = Coord(maze.head.indexOf('|'), 0)
    LazyList.unfold((start, Direction.Up)) { (coord, direction) =>
      for
        char <- maze.get(coord)
        if !char.isWhitespace
        next <- char match
          case '+' =>
            Direction.hvOnly
              .filterNot(d => Set(d, d.oposite).contains(direction))
              .find(d => maze.get(coord + d.direction).exists(!_.isWhitespace))
          case _ => Some(direction)
      yield char -> (coord + next.direction, next)
    }

  def solve(input: String): String =
    parseInput(input).filter(_.isLetter).mkString

  def solve2(input: String): Int =
    parseInput(input).size
