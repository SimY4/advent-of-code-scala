package AdventOfCode
package y2018

object Day1 {

  val input = """+1
                |+1
                |+1""".stripMargin

  def calibrate(str: String): Int = str.linesIterator.map(_.toInt).foldLeft(0)(_ + _)

  sealed trait State
  case class NotFound(st: Int, viewed: Set[Int]) extends State
  case class Found(st: Int) extends State

  def calibrate2(str: String): Int = {
    val init: State = NotFound(0, Set.empty)
    def infinite: Stream[String] = str.linesIterator.toStream #::: infinite
    infinite
      .scanLeft(init) { (state, cal) =>
        state match {
          case s @ Found(_) => s
          case NotFound(acc, set) =>
            val nextCal = acc + cal.toInt
            if (set contains nextCal) Found(nextCal) else NotFound(nextCal, set + nextCal)
        }
      }
      .collectFirst { case Found(st) => st }
      .get
  }

}
