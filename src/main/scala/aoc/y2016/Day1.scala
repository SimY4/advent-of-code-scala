package aoc
package y2016

object Day1:
  import Direction.*

  private def navigate(direction: Direction, instruction: String): (Direction & HV, List[Direction & HV]) =
    val steps = instruction.tail.toInt
    val nextDirection = (instruction.head, direction) match
      case ('R', Up)    => Right
      case ('R', Right) => Down
      case ('R', Down)  => Left
      case ('R', Left)  => Up
      case ('L', Up)    => Left
      case ('L', Right) => Up
      case ('L', Down)  => Right
      case ('L', Left)  => Down
    nextDirection -> List.fill(steps)(nextDirection)

  def solve(input: String): Long =
    val (_, Coord(x, y)) = input
      .split(", ")
      .foldLeft(Up -> Coord(0L, 0L)) { (state, instruction) =>
        val (direction, coords) = navigate(state._1, instruction)
        direction -> coords.foldLeft(state._2)(_ + _.direction)
      }
    math.abs(x) + math.abs(y)

  def solve2(input: String): Option[Long] =
    val (_, path) = input
      .split(", ")
      .foldLeft(Up -> List(Coord(0L, 0L))) { (state, instruction) =>
        val (direction, coords) = navigate(state._1, instruction)
        direction -> coords.foldRight(state._2)((coord, list) => (coord.direction + list.head) :: list)
      }
    path.tails.collectFirst {
      case Coord(x, y) :: tail if tail.size == tail.toSet.size =>
        math.abs(x) + math.abs(y)
    }

  val input =
    """R4, R5, L5, L5, L3, R2, R1, R1, L5, R5, R2, L1, L3, L4, R3, L1, L1, R2, R3, R3, R1, L3, L5, R3, R1, L1, R1, R2, L1, L4, L5, R4, R2, L192, R5, L2, R53, R1, L5, R73, R5, L5, R186, L3, L2, R1, R3, L3, L3, R1, L4, L2, R3, L5, R4, R3, R1, L1, R5, R2, R1, R1, R1, R3, R2, L1, R5, R1, L5, R2, L2, L4, R3, L1, R4, L5, R4, R3, L5, L3, R4, R2, L5, L5, R2, R3, R5, R4, R2, R1, L1, L5, L2, L3, L4, L5, L4, L5, L1, R3, R4, R5, R3, L5, L4, L3, L1, L4, R2, R5, R5, R4, L2, L4, R3, R1, L2, R5, L5, R1, R1, L1, L5, L5, L2, L1, R5, R2, L4, L1, R4, R3, L3, R1, R5, L1, L4, R2, L3, R5, R3, R1, L3"""
