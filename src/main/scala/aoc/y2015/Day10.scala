package aoc.y2015

object Day10:
  def solve(input: String, times: Int = 40): Int =
    (1 to times)
      .foldLeft(input.toList) { (list, _) =>
        list
          .foldRight(Nil: List[(Char, Int)]) { (ch, list) =>
            list match
              case (c, cnt) :: tail if c == ch => (ch, cnt + 1) :: tail
              case _                           => (ch, 1) :: list
          }
          .flatMap((ch, cnt) => s"$cnt$ch".toList)
      }
      .size

  def solve2(input: String): Int = solve(input, 50)

  val input = "1113222113"
