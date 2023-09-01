package aoc.y2015

object Day24:
  def solve(input: String, nGroups: Int = 3): Long =
    val weights = input.linesIterator
      .map(_.toLong)
      .toVector
    val maxWeight = weights.sum / nGroups

    val tooSmall = weights.inits.dropWhile(_.sum >= maxWeight).flatMap(_.lastOption).toVector
    val tooBig   = weights.tails.dropWhile(_.sum >= maxWeight).flatMap(_.headOption).toVector
    (for
      i     <- Iterator.range(tooBig.size, tooSmall.size)
      group <- weights.combinations(i)
      if group.sum == maxWeight
    yield group.product).next

  def solve2(input: String): Long = solve(input, 4)

  val input = """1
                |3
                |5
                |11
                |13
                |17
                |19
                |23
                |29
                |31
                |41
                |43
                |47
                |53
                |59
                |61
                |67
                |71
                |73
                |79
                |83
                |89
                |97
                |101
                |103
                |107
                |109
                |113""".stripMargin
