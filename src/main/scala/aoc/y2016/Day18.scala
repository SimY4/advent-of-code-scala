package aoc.y2016

object Day18 {
  def solve(input: String, n: Int = 40): Int = 
    LazyList.iterate(input.toVector) { row =>
      (0 until row.size)
        .map {
          case i if row.lift(i - 1).exists(_ == '^') && row(i) == '^' && row.lift(i + 1).forall(_ == '.') => '^'
          case i if row.lift(i - 1).forall(_ == '.') && row(i) == '^' && row.lift(i + 1).exists(_ == '^') => '^'
          case i if row.lift(i - 1).forall(_ == '.') && row(i) == '.' && row.lift(i + 1).exists(_ == '^') => '^'
          case i if row.lift(i - 1).exists(_ == '^') && row(i) == '.' && row.lift(i + 1).forall(_ == '.') => '^'
          case _ => '.'
        }
        .toVector
    }
    .take(n)
    .map(_.count(_ == '.'))
    .sum

  def solve2(input: String): Int = solve(input, 400000)

  val input = "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."
}
