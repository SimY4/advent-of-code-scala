package AdventOfCode
package y2018

import scala.annotation.tailrec

object Day5 {

  val input = """dabAcCaCBAcCcaDA""".stripMargin

  val regex = ('a' to 'z')
    .flatMap { c =>
      Seq(s"$c" + s"${c.toUpper}", s"${c.toUpper}" + s"$c")
    }
    .mkString("(", "|", ")")
    .r

  @tailrec def solve0(acc: String): String =
    regex.replaceFirstIn(acc, "") match {
      case res if acc == res => res
      case res               => solve0(res)
    }

  def solve(input: String): Int = solve0(input).length

  def solve2(input: String): Int =
    (for {
      ch <- ('a' to 'z').par
      filteredInput = s"[$ch${ch.toUpper}]".r.replaceAllIn(input, "")
    } yield (solve0(filteredInput).length)).min

}
