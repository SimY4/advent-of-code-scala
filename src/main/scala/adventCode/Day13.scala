package adventCode

import scala.annotation.tailrec

object Day13 extends App {
  import adventCode.day13_1.InputParser

  case class Row(range: Int, pos: Int = 0, direction: Int = 1)
  type Firewall = Map[Int, Row]
//  implicit def firewallOps(firewall: Firewall): FirewallOps = new FirewallOps(firewall)
//  final class FirewallOps(firewall: Firewall) {
//    def nextState: Firewall = for {
//      (k, row) <- firewall
//      next = if (r < s)
//    } yield k -> v
//  }

  private val input = """0: 3
                        |1: 2
                        |4: 4
                        |6: 4""".stripMargin

//  def severity(input: Firewall): Int = {
//    @tailrec def severity0(severity: Int, state: (Int, Firewall)): Int = if (state._1 < state._2.size) {
//      val nextState = state
//    } else {
//      severity
//    }
//
//    severity0(0, 0 -> input)
//  }
  println(new InputParser("11 1 1\r\n01 1\r\n00 ").Input.run())
}

package day13 {
  import adventCode.Day13.{ Firewall, Row }
  import org.parboiled2._

  class InputParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Firewall] = rule { Lines ~ EOI }

    private def Lines = rule { oneOrMore(Record).separatedBy(NewLine) ~> (_.toMap) }

    private def Record = rule { Number ~ ": " ~ Number ~> (_ -> Row(_)) }

    private def Number = rule { capture(oneOrMore(CharPredicate.Digit)) ~> (_.toInt) }

    private def NewLine = rule { optional('\r') ~ '\n' }

  }

}

package day13_1 {
  import org.parboiled2._

  sealed trait Line
  case object Line00 extends Line
  case class Line10(first: Int) extends Line
  case class Line01(second: Int) extends Line
  case class Line11(first: Int, second: Int) extends Line

  class InputParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Seq[Line]] = rule { Lines ~ EOI }

    private def Lines = rule { oneOrMore(Record).separatedBy(NewLine) }

    private def Record = rule { Number ~ First ~ Second }

    private def First = rule { run { mask: Int => if (???) (' ' ~ Number) else "" } }

    private def Second =  rule { run { mask: Int => if (???) ' ' ~ Number else "" } }

    private def Number = rule { capture(oneOrMore(CharPredicate.Digit)) ~> (_.toInt) }

    private def NewLine = rule { optional('\r') ~ '\n' }

  }


}
