package aoc.y2022

object Day10:
  private enum Ins:
    case Noop
    case AddX(n: Int)
  import Ins.*

  def solve(input: String): Int =
    input.linesIterator
      .flatMap:
        case "noop"     => Noop :: Nil
        case s"addx $n" => Noop :: AddX(n.toInt) :: Nil
      .scanLeft(1 -> 0):
        case ((x, before), Noop)    => (x + before) -> 0
        case ((x, before), AddX(n)) => (x + before) -> n
      .zipWithIndex
      .filter((_, cycle) => List(20, 60, 100, 140, 180, 220).contains(cycle))
      .map(_._1 * _)
      .sum

  def solve2(input: String): String =
    input.linesIterator
      .flatMap:
        case "noop"     => Noop :: Nil
        case s"addx $n" => Noop :: AddX(n.toInt) :: Nil
      .foldLeft((1, 0, java.lang.StringBuilder())):
        case ((x, before, sb), Noop) =>
          val newX = x + before
          (newX, 0, sb.append(if newX - 1 <= sb.length() % 40 && sb.length() % 40 <= newX + 1 then '#' else '.'))
        case ((x, before, sb), AddX(n)) =>
          val newX = x + before
          (newX, n, sb.append(if newX - 1 <= sb.length() % 40 && sb.length() % 40 <= newX + 1 then '#' else '.'))
      ._3
      .insert(200, '\n')
      .insert(160, '\n')
      .insert(120, '\n')
      .insert(80, '\n')
      .insert(40, '\n')
      .insert(0, '\n')
      .toString()

  val input = """noop
                |noop
                |noop
                |addx 5
                |noop
                |addx 1
                |addx 2
                |addx 5
                |addx 2
                |addx 1
                |noop
                |addx 5
                |noop
                |addx -1
                |noop
                |addx 5
                |noop
                |noop
                |addx 5
                |addx 1
                |noop
                |noop
                |addx 3
                |addx 2
                |noop
                |addx -38
                |noop
                |addx 3
                |addx 2
                |addx -5
                |addx 12
                |addx 2
                |addx 27
                |addx -40
                |addx 19
                |addx 2
                |addx 19
                |addx -18
                |addx 2
                |addx 5
                |addx 2
                |addx -23
                |addx 22
                |addx 4
                |addx -34
                |addx -1
                |addx 5
                |noop
                |addx 2
                |addx 1
                |addx 20
                |addx -17
                |noop
                |addx 25
                |addx -17
                |addx -2
                |noop
                |addx 3
                |addx 19
                |addx -12
                |addx 3
                |addx -2
                |addx 3
                |addx 1
                |noop
                |addx 5
                |noop
                |noop
                |addx -37
                |addx 3
                |addx 4
                |noop
                |addx 24
                |addx -6
                |addx -15
                |addx 2
                |noop
                |addx 6
                |addx -2
                |addx 6
                |addx -12
                |addx -2
                |addx 19
                |noop
                |noop
                |noop
                |addx 3
                |noop
                |addx 7
                |addx -2
                |addx -24
                |addx -11
                |addx 4
                |addx 3
                |addx -2
                |noop
                |addx 7
                |addx -2
                |addx 2
                |noop
                |addx 3
                |addx 7
                |noop
                |addx -2
                |addx 5
                |addx 2
                |addx 5
                |noop
                |noop
                |noop
                |addx 3
                |addx -35
                |addx 35
                |addx -21
                |addx -14
                |noop
                |addx 5
                |addx 2
                |addx 33
                |addx -7
                |addx -23
                |addx 5
                |addx 2
                |addx 1
                |noop
                |noop
                |addx 5
                |addx -1
                |noop
                |addx 3
                |addx -23
                |addx 30
                |addx 1
                |noop
                |addx 4
                |addx -17
                |addx 11
                |noop
                |noop""".stripMargin
