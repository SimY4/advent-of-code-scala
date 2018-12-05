package AdventOfCode
package y2017

import scala.annotation.tailrec

object Day12 {
  import day12.InputParser

  type Group = Set[Int]

  private val input = """0 <-> 2
                        |1 <-> 1
                        |2 <-> 0, 3, 4
                        |3 <-> 2, 4
                        |4 <-> 2, 3, 6
                        |5 <-> 6
                        |6 <-> 4, 5""".stripMargin

  def group(program: Int, input: Map[Int, Seq[Int]]): Group = {
    @tailrec def group0(acc: Set[Int], current: Set[Int]): Group = if (current.isEmpty) { acc } else {
      val connects = acc ++ current.flatMap(input)
      group0(connects, connects.diff(acc))
    }

    group0(Set(program), Set(program))
  }

  println(for {
    map <- new InputParser(input).Input.run()
    gr = group(0, map)
    diff = map.keySet.diff(gr)
  } yield diff.size -> (map.keySet.size - diff.size))

  // PART 2

  def groups(input: Map[Int, Seq[Int]]): Set[Group] = {
    @tailrec def groups0(acc: Set[Group], i: Map[Int, Seq[Int]]): Set[Group] = if (i.isEmpty) { acc } else {
      val groups = acc + group(i.keySet.head, i)
      groups0(groups, i -- groups.flatten)
    }

    groups0(Set.empty, input)
  }

  println(for {
    map <- new InputParser(input).Input.run()
    grs = groups(map)
  } yield grs.size)

}

package day12 {
  import org.parboiled2._

  class InputParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Map[Int, Seq[Int]]] = rule { Lines ~ EOI }

    private def Lines = rule { oneOrMore(Pipe).separatedBy(NewLine) ~> (_.toMap) }

    private def Pipe = rule { Program ~ " <-> " ~ oneOrMore(Program).separatedBy(", ") ~> (_ -> _) }

    private def Program = rule { capture(oneOrMore(CharPredicate.Digit)) ~> (_.toInt) }

    private def NewLine = rule { optional('\r') ~ '\n' }

  }

}
