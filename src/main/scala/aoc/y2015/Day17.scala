package aoc.y2015

object Day17 {
  def solve(input: String): Int = {
    val containers = input.linesIterator.map(_.toInt).toList
    
    (0 until (1 << containers.size))
      .count { mask => 
        (for
          (j, i) <- containers.zipWithIndex
          if (mask & (1 << i)) > 0
        yield j)
          .sum == 150
      }
  }

  def solve2(input: String): Int = {
    val containers = input.linesIterator.map(_.toInt).toList
    
    (0 until (1 << containers.size))
      .flatMap { mask => 
        val set = (for
          (j, i) <- containers.zipWithIndex
          if (mask & (1 << i)) > 0
        yield j)
        Option.when(set.sum == 150) { set.size }
      }
      .groupBy(identity)
      .toSeq
      .minBy(_._1)
      ._2.size
  }

  val input = """33
                |14
                |18
                |20
                |45
                |35
                |16
                |35
                |1
                |13
                |18
                |13
                |50
                |44
                |48
                |6
                |24
                |41
                |30
                |42""".stripMargin
}