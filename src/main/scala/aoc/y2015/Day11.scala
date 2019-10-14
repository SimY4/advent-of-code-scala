package aoc.y2015

import scala.annotation.tailrec

object Day11

  private val forbidden = Set('i', 'o', 'l')
  private val triples = ('a' to 'x')
    .map { ch => s"$ch${(ch + 1).toChar}${(ch + 2).toChar}" }
    .toSet
  private val pairs = ('a' to 'z').map { ch => s"$ch$ch" }.toList

  private def meetRequirements(pass: String): Boolean =
    (for
      p <- Some(pass)
      if p.sliding(3).exists(triples.contains)
      if !p.exists(forbidden.contains)
      if pairs.mkString("|", "(", ")[2]").r.findAllIn(p).nonEmpty
    yield ()).isDefined

  private def (s: String) increment: String =
    s.reverse.toList match
      case 'z' :: tail => increment(tail.reverse.mkString) + 'a'
      case x :: tail => ((x + 1).toChar :: tail).reverse.mkString
      case Nil => "a"

  def solve(input: String): String =
    @tailrec def iterate(input: String): String =
      if (meetRequirements(input)) input
      else iterate(increment(input))

    iterate(input)
    
  def solve2(input: String): Int = ???
