package aoc.y2017

object Day14:
  import Day10.solve2 as knotHash

  def solve(input: String): Int =
    (for
      i <- 0 to 127
      hash = knotHash(s"$input-$i", _.toBinaryString)
    yield hash.count(_ == '1')).sum

  def solve2(input: String): Int =
    val matrix = (for
      i <- 0 to 127
      hash = knotHash(s"$input-$i", _.toBinaryString.reverse.padTo(8, '0').reverse)
    yield hash.toCharArray).toArray

    def remove(i: Int, j: Int): Int =
      if i >= 0 && j >= 0 && i < 128 && j < 128 && matrix(i)(j) == '1' then
        matrix(i)(j) = '0'
        1 + remove(i + 1, j) * remove(i - 1, j) * remove(i, j + 1) * remove(i, j - 1)
      else 0

    (for
      i <- 0 to 127
      j <- 0 to 127
    yield remove(i, j)).sum

  val input = "ljoxqyyw"
