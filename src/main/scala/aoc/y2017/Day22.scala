package aoc
package y2017

object Day22:
  private enum Mode extends Enum[Mode]:
    case Weakened, Infected, Flagged

  private case class State(grid: Map[Coord, Mode], pos: Coord, dir: Direction & HV)

  def solve(input: String, n: Int = 10000): Int =
    val center = input.linesIterator.size / 2
    val grid = (for
      (line, j) <- input.linesIterator.zipWithIndex
      (c, i)    <- line.zipWithIndex
      if c == '#'
    yield Coord(i - center, j - center) -> Mode.Infected).toMap

    LazyList
      .unfold(State(grid, Coord(0L, 0L), Direction.Down)) { case State(grid, pos, dir) =>
        val infected = grid.contains(pos)
        val nextDir  = if infected then dir.left else dir.right
        val nextGrid = grid.updatedWith(pos):
          case Some(_) => None
          case None    => Some(Mode.Infected)
        val nextPos = pos + nextDir.direction
        Some(!infected -> State(nextGrid, nextPos, nextDir))
      }
      .take(n)
      .count(identity)

  def solve2(input: String, n: Int = 10000000): Int =
    val center = input.linesIterator.size / 2
    val grid = (for
      (line, j) <- input.linesIterator.zipWithIndex
      (c, i)    <- line.zipWithIndex
      if c == '#'
    yield Coord(i - center, j - center) -> Mode.Infected).toMap

    LazyList
      .unfold(State(grid, Coord(0L, 0L), Direction.Down)) { case State(grid, pos, dir) =>
        val infected = grid.get(pos).exists(_ == Mode.Weakened)
        val nextDir = grid.get(pos) match
          case Some(Mode.Flagged)  => dir.oposite.asInstanceOf[Direction & HV]
          case Some(Mode.Infected) => dir.left
          case Some(Mode.Weakened) => dir
          case None                => dir.right
        val nextGrid = grid.updatedWith(pos):
          case Some(Mode.Flagged)  => None
          case Some(Mode.Infected) => Some(Mode.Flagged)
          case Some(Mode.Weakened) => Some(Mode.Infected)
          case None                => Some(Mode.Weakened)
        val nextPos = pos + nextDir.direction
        Some(infected -> State(nextGrid, nextPos, nextDir))
      }
      .take(n)
      .count(identity)

  val input = """..##.##.######...#.######
                |##...#...###....##.#.#.##
                |###.#.#.#..#.##.####.#.#.
                |..##.##...#..#.##.....##.
                |##.##...#.....#.#..#.####
                |.###...#.........###.####
                |#..##....###...#######..#
                |###..#.####.###.#.#......
                |.#....##..##...###..###.#
                |###.#..#.##.###.#..###...
                |####.#..##.#.#.#.#.#...##
                |##.#####.#......#.#.#.#.#
                |..##..####...#..#.#.####.
                |.####.####.####...##.#.##
                |#####....#...#.####.#..#.
                |.#..###..........#..#.#..
                |.#.##.#.#.##.##.#..#.#...
                |..##...#..#.....##.####..
                |..#.#...######..##..##.#.
                |.####.###....##...####.#.
                |.#####..#####....####.#..
                |###..#..##.#......##.###.
                |.########...#.#...###....
                |...##.#.##.#####.###.####
                |.....##.#.#....#..#....#.""".stripMargin
