package aoc.y2023

object Day6:
  def solve(input: List[(Int, Int)]): Int =
    (for (time, distance) <- input
    yield (0 until time).count(t => t * (time - t) > distance)).product

  def solve2(input: List[(Int, Int)]): Int =
    val (time, distance) = input.foldRight(("", "")):
      case ((t, d), (at, ad)) => (t.toString + at, d.toString + ad)

    (0 until time.toInt).count(t => t * (time.toInt - t) > distance.toInt)

  val input = List((41, 244), (66, 1047), (72, 1228), (66, 1040))
