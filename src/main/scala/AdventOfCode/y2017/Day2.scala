package AdventOfCode
package y2017

object Day2 {

  private val spreadsheet = """5 1 9 5
                              |7 5 3
                              |2 4 6 8""".stripMargin

  def checksum(spreadsheet: String): Int =
    (for {
      line <- spreadsheet.linesIterator
      arr = line.split("\\s+").map(_.toInt)
      min = arr.min
      max = arr.max
    } yield max - min).sum

  println(checksum(spreadsheet) == 18)

  // PART 2

  private val spreadsheet2 = """5 9 2 8
                               |9 4 7 3
                               |3 8 6 5""".stripMargin

  def checksum2(spreadsheet: String): Int =
    (for {
      line <- spreadsheet.linesIterator
      arr = line.split("\\s+").map(_.toInt)
      x1 <- arr
      x2 <- arr
      if x1 != x2 && x1 % x2 == 0
    } yield x1 / x2).sum

  println(checksum2(spreadsheet2) == 9)

}
