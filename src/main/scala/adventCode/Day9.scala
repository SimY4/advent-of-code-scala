package adventCode

object Day9 {
  import day9.StreamParser

  type Score = (Int, Int)
  implicit def scoreOps(score: Score): ScoreOps = new ScoreOps(score)

  final class ScoreOps(score: Score) {
    def +(other: Score): Score = (score._1 + other._1) -> (score._2 + score._2)
  }

  type \/[+A, +B] = Either[A, B]
  sealed trait Group
  case class NestedGroup(groups: (Garbage \/ Group)*) extends Group
  case class SimpleGroup(value: String) extends Group
  case class Garbage(value: String)


  private val inputs = Seq(
    "<>",
    "<random characters>",
    "<<<<>",
    "<{!>}>",
    "<!!>",
    "<!!!>>",
    "<{o\"i!a,<{i<a>",

    "{}",
    "{{{}}}",
    "{{},{}}",
    "{{{},{},{{}}}}",
    "{<{},{},{{}}>}",
    "{<a>,<a>,<a>,<a>}",
    "{{<a>},{<a>},{<a>},{<a>}}",
    "{{<!>},{<!>},{<!>},{<a>}}"
  )


  println(for {
    input <- inputs
    run = new StreamParser(input).Input.run()
    _ = println(run)
    score <- run.toOption
  } yield score)

}

package day9 {
  import adventCode.Day9.{ \/, scoreOps }
  import org.parboiled2._

  class StreamParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Day9.Score] = rule { GroupOrGarbage ~ EOI ~> { gg: Day9.Garbage \/ Day9.Group => score(0 -> 0, gg) } }

    private def GroupOrGarbage: Rule1[Day9.Garbage \/ Day9.Group] = rule { Group ~> (Right(_)) | Garbage ~> { g: Day9.Garbage => Left(g) } }

    private def Group = rule { EmptyGroup | '{' ~ oneOrMore(GroupOrGarbage).separatedBy(',') ~> (Day9.NestedGroup(_ :_*)) ~ '}' }

    private def EmptyGroup = rule { "{}" ~ push(Day9.SimpleGroup("")) }

    private def Garbage = rule { '<' ~ capture(zeroOrMore(EscapedChar | (!'>' ~ CharPredicate.All))) ~ '>' ~> Day9.Garbage }

    private def EscapedChar = rule { '!' ~ CharPredicate.All }

    private def score(acc: Day9.Score, g: Day9.Garbage \/ Day9.Group): Day9.Score = g match {
      case Right(Day9.NestedGroup(groupsOrGarbage @ _*)) =>
        val next = acc._1 + 1 -> acc._2
        groupsOrGarbage.foldLeft(next) { (a, gg) =>
          val s = score(next._1 -> 0, gg)
          a + s
        }
      case Right(Day9.SimpleGroup(_)) => acc._1 + 1 -> 0
      case Left(Day9.Garbage(garbage)) => 0 -> (acc._2 + garbage.replaceAll("!.", "").length)
    }

  }

}
