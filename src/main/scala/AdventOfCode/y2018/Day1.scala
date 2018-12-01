package AdventOfCode
package y2018

object Day1 {

  val input = """+1
                |+1
                |+1""".stripMargin

  def calibrate(str: String): Int = str.linesIterator.foldLeft(0) { (acc, cal) => 
    val charSeq: Seq[Char] = cal
    charSeq match {
      case Seq('+', cs @ _*) => acc + cs.mkString.toInt
      case Seq('-', cs @ _*) => acc - cs.mkString.toInt
    }
  }

  sealed trait State
  case class NotFound(st: Int, viewed: Set[Int]) extends State
  case class Found(st: Int) extends State

  def calibrate2(str: String): Int = {
    val init: State = NotFound(0, Set.empty[Int])
    def infinite: Stream[String] = str.linesIterator.toStream #::: infinite
    infinite.scanLeft(init) { (state, cal) => 
      state match {
        case s @ Found(_) => s
        case NotFound(acc, set) =>
          val charSeq: Seq[Char] = cal
          charSeq match {
            case Seq('+', cs @ _*) => 
              val nextCal = acc + cs.mkString.toInt
              if (set contains nextCal) Found(nextCal) else NotFound(nextCal, set + nextCal)
            case Seq('-', cs @ _*) => 
              val nextCal = acc - cs.mkString.toInt
              if (set contains nextCal) Found(nextCal) else NotFound(nextCal, set + nextCal)
          }
      }
    }
    .collectFirst { case Found(st) => st }
    .get
  }

}