package aoc.y2015

object Day11:
  private val forbidden = "iol"

  private def increment(s: Vector[Char]): Vector[Char] =
    s.lastOption match
      case Some('z') => increment(s.init).appended('a')
      case Some(x)   => s.init.appended((x.toInt + 1).toChar)
      case None      => Vector('a')

  private def meetRequirements(pass: Vector[Char]): Boolean =
    (pass
      .sliding(3)
      .exists: triple =>
        triple.size == 3 && triple(0) + 1 == triple(1) && triple(1) + 1 == triple(2))
      && !pass.exists(forbidden.contains)
      && pass
        .sliding(2)
        .zipWithIndex
        .collect:
          case (pair, idx) if pair.size == 2 && pair(0) == pair(1) => idx
        .sliding(2)
        .exists(pair => pair.size == 2 && pair(0) + 1 != pair(1))

  def solve(input: String): String =
    LazyList
      .iterate(input.toVector)(increment)
      .filter(meetRequirements)
      .head
      .mkString

  def solve2(input: String): String =
    LazyList
      .iterate(input.toVector)(increment)
      .filter(meetRequirements)
      .drop(1)
      .head
      .mkString

  val input = "hxbxwxba"
