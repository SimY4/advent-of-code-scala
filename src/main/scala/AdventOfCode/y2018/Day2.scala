package AdventOfCode
package y2018

object Day2 {

  val input = """abcde
                |fghij
                |klmno
                |pqrst
                |fguij
                |axcye
                |wvxyz""".stripMargin

  def solve(input: String): Int = {
    val count = input.linesIterator.foldLeft(0 -> 0) { (acc, line) =>
      val grouped = line.groupBy(identity).values.map(_.length)
      val acc0 = if (grouped.exists(_ == 2)) (acc._1 + 1, acc._2) else acc
      if (grouped.exists(_ == 3)) (acc0._1, acc0._2 + 1) else acc0
    }
    count._1 * count._2
  }

  def solve2(input: String): String =
    (for {
      line1 <- input.linesIterator
      line2 <- input.linesIterator
      if line1 < line2
      indexes = (line1.zip(line2) zipWithIndex).collect { case ((l, r), i) if l != r => i }
      if indexes.size == 1
      index <- indexes.headOption
    } yield (line1.substring(0, index) + line1.substring(index + 1))).toList.head

}
