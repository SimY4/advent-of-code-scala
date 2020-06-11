package aoc
package y2016

object Day1 {
  private enum Direction {
    case N, E, S, W
  }
  import Direction._

  private def navigate(direction: Direction, instruction: String): (Direction, List[Coord]) = {
    val steps = instruction.tail.toInt
    val nextDirection = (instruction.head, direction) match {
      case ('R', N) => E
      case ('R', E) => S
      case ('R', S) => W
      case ('R', W) => N
      case ('L', N) => W
      case ('L', E) => N
      case ('L', S) => E
      case ('L', W) => S
    }
    nextDirection -> List.fill(steps) { 
      nextDirection match {
        case N => Coord(0L, 1L)
        case E => Coord(1L, 0L)
        case S => Coord(0L, -1L)
        case W => Coord(-1L, 0L)
      }
    }
  }

  def solve(input: String): Long = {
    val Coord(x, y) = input.split(", ").foldLeft(N -> Coord(0L, 0L)) { (state, instruction) => 
      val (direction, coords) = navigate(state._1, instruction)
      direction -> coords.foldLeft(state._2)(_ + _)
    }._2
    math.abs(x) + math.abs(y)
  }

  def solve2(input: String): Option[Long] = {
    input.split(", ").foldLeft(N -> List(Coord(0L, 0L))) { (state, instruction) => 
      val (direction, coords) = navigate(state._1, instruction)
      direction -> coords.foldRight(state._2) { (coord, list) => (coord + list.head) :: list } 
    }._2
      .tails
      .collectFirst { case head :: tail if tail.size == tail.toSet.size => head }
      .map { case Coord(x, y) => math.abs(x) + math.abs(y) }
  }

  val input = """R4, R5, L5, L5, L3, R2, R1, R1, L5, R5, R2, L1, L3, L4, R3, L1, L1, R2, R3, R3, R1, L3, L5, R3, R1, L1, R1, R2, L1, L4, L5, R4, R2, L192, R5, L2, R53, R1, L5, R73, R5, L5, R186, L3, L2, R1, R3, L3, L3, R1, L4, L2, R3, L5, R4, R3, R1, L1, R5, R2, R1, R1, R1, R3, R2, L1, R5, R1, L5, R2, L2, L4, R3, L1, R4, L5, R4, R3, L5, L3, R4, R2, L5, L5, R2, R3, R5, R4, R2, R1, L1, L5, L2, L3, L4, L5, L4, L5, L1, R3, R4, R5, R3, L5, L4, L3, L1, L4, R2, R5, R5, R4, L2, L4, R3, R1, L2, R5, L5, R1, R1, L1, L5, L5, L2, L1, R5, R2, L4, L1, R4, R3, L3, R1, R5, L1, L4, R2, L3, R5, R3, R1, L3"""
}