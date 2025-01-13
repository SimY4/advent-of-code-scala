package aoc.y2024

import scala.collection.mutable

object Day11:
  def solve(input: String, times: Int = 25): Long =
    val stones = "\\d+".r.findAllIn(input).map(_.toLong).toVector

    val cache = mutable.Map.empty[(Long, Int), Long]

    def loop(stone: Long, iteration: Int): Long =
      if iteration == 0 then 1L
      else
        cache.getOrElseUpdate(
          stone -> iteration,
          if stone == 0 then loop(1, iteration - 1)
          else
            val n = math.log10(stone.toDouble).toInt + 1
            if (n & 1) == 0 then
              loop(stone % math.pow(10, n / 2).toLong, iteration - 1) +
                loop(stone / math.pow(10, n / 2).toLong, iteration - 1)
            else loop(stone * 2024L, iteration - 1)
        )

    stones.map(loop(_, times)).sum

  def solve2(input: String): Long = solve(input, 75)

  val input = "4189 413 82070 61 655813 7478611 0 8"
