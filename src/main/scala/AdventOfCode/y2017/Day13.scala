package AdventOfCode
package y2017

import scala.annotation.tailrec

object Day13 extends App {
  import day13.InputParser

  case class Row(range: Int, pos: Int = 0, direction: Int = 1)
  type Firewall = Map[Int, Row]

  private val input = """0: 3
                        |1: 2
                        |4: 4
                        |6: 4""".stripMargin

  println(new InputParser("11 1 1\r\n01 1\r\n00 ").Input.run())
}

package day13 {
  import Day13.{ Firewall, Row }
  import org.parboiled2._

  class InputParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Firewall] = rule { Lines ~ EOI }

    private def Lines = rule { oneOrMore(Record).separatedBy(NewLine) ~> (_.toMap) }

    private def Record = rule { Number ~ ": " ~ Number ~> (_ -> Row(_)) }

    private def Number = rule { capture(oneOrMore(CharPredicate.Digit)) ~> (_.toInt) }

    private def NewLine = rule { optional('\r') ~ '\n' }

  }

}
