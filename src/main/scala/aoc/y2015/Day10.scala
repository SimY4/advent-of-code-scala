package aoc.y2015

object Day10:
  def solve(input: String, times: Int = 40): Int =
    LazyList
      .iterate(input.toList): list =>
        list
          .foldRight(Nil: List[(Char, Int)]):
            case (ch, (c, cnt) :: tail) if c == ch => (ch, cnt + 1) :: tail
            case (ch, list)                        => (ch, 1) :: list
          .flatMap((ch, cnt) => s"$cnt$ch".toList)
      .drop(times)
      .head
      .size

  def solve2(input: String): Int = solve(input, 50)

  val input = "1113222113"
