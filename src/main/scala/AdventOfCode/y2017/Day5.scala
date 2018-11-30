package AdventOfCode
package y2017

import scala.annotation.tailrec

object Day5 {

  private val instrucitons = """0
                               |3
                               |0
                               |1
                               |-3""".stripMargin

  def countHops(instructions: String): Int = {
    @tailrec def countHops0(count: Int, list: List[Int], index: Int): Int = {
      val hop = list(index)

      if (index + hop >= list.size || index + hop < 0)
        count
      else
        countHops0(count + 1, list.updated(index, hop + 1), index + hop)
    }

    countHops0(1, instructions.linesIterator.map(_.toInt).toList, 0)
  }

  println(countHops(instrucitons) == 5)

  // PART 2

  def countHops2(instructions: String): Long = {
    @tailrec def countHops20(count: Long, list: List[Int], index: Int): Long = {
      val hop = list(index)

      if (index + hop >= list.size || index + hop < 0)
        count
      else {
        val offset = if (hop >= 3) hop - 1 else hop + 1
        countHops20(count + 1, list.updated(index, offset), index + hop)
      }
    }

    countHops20(1L, instructions.linesIterator.map(_.toInt).toList, 0)
  }

  println(countHops2(instrucitons) == 10)

}
