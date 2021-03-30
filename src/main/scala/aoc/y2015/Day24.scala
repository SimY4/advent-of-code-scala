package aoc.y2015

object Day24 {
  def solve(input: String, nGroups: Int = 3): Long = {
    val weights = input.linesIterator
      .map(_.toInt)
      .toList
    val maxWeight = weights.sum / nGroups

    def groups: Seq[List[Int]] = {
      val tooSmall = input.linesIterator.map(_.toInt).toList.inits.dropWhile(_.sum >= maxWeight).flatMap(_.lastOption).toList
      val tooBig =  input.linesIterator.map(_.toInt).toList.tails.dropWhile(_.sum >= maxWeight).flatMap(_.headOption).toList
      for
        i <- tooBig.size until tooSmall.size
        group <- weights.combinations(i)
        if group.sum == maxWeight
      yield group
    }

    groups
      .take(1)
      .foldLeft(Int.MaxValue -> List.empty[List[Int]]) { (acc, group) => 
        val compare = acc._1 compare group.size
        if compare < 0 then acc
        else if compare > 0 then group.size -> (group :: Nil)
        else acc._1 -> (group :: acc._2)
      }
      ._2
      .map(_.map(_.toLong).product)
      .max
  }

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
}
