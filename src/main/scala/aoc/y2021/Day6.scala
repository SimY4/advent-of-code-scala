package aoc.y2021

object Day6:
  def solve(input: String): Int =
    LazyList
      .iterate(input.split(',').map(_.toInt).toList) { days =>
        val updated = days.map(_ - 1).map { day =>
          if (day < 0) 6 else day
        }
        updated ::: List.fill(days.count(_ - 1 < 0))(8)
      }
      .drop(80)
      .head
      .size

  def solve2(input: String): Long =
    val days   = input.split(',').map(_.toInt).toList
    val counts = (0 to 8).map(i => days.count(_ == i).toLong).toVector
    (0 until 256)
      .foldLeft(counts) { (counts, _) =>
        val zeros = counts.head
        val next  = counts.tail :+ zeros
        next.updated(6, next(6) + zeros)
      }
      .sum

  val input =
    """3,4,3,1,2,1,5,1,1,1,1,4,1,2,1,1,2,1,1,1,3,4,4,4,1,3,2,1,3,4,1,1,3,4,2,5,5,3,3,3,5,1,4,1,2,3,1,1,1,4,1,4,1,5,3,3,1,4,1,5,1,2,2,1,1,5,5,2,5,1,1,1,1,3,1,4,1,1,1,4,1,1,1,5,2,3,5,3,4,1,1,1,1,1,2,2,1,1,1,1,1,1,5,5,1,3,3,1,2,1,3,1,5,1,1,4,1,1,2,4,1,5,1,1,3,3,3,4,2,4,1,1,5,1,1,1,1,4,4,1,1,1,3,1,1,2,1,3,1,1,1,1,5,3,3,2,2,1,4,3,3,2,1,3,3,1,2,5,1,3,5,2,2,1,1,1,1,5,1,2,1,1,3,5,4,2,3,1,1,1,4,1,3,2,1,5,4,5,1,4,5,1,3,3,5,1,2,1,1,3,3,1,5,3,1,1,1,3,2,5,5,1,1,4,2,1,2,1,1,5,5,1,4,1,1,3,1,5,2,5,3,1,5,2,2,1,1,5,1,5,1,2,1,3,1,1,1,2,3,2,1,4,1,1,1,1,5,4,1,4,5,1,4,3,4,1,1,1,1,2,5,4,1,1,3,1,2,1,1,2,1,1,1,2,1,1,1,1,1,4""".stripMargin
