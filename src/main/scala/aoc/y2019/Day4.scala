package aoc.y2019

object Day4:
  def solve(input: String): Int =
    val min :: max :: Nil = "\\d+".r.findAllIn(input).map(_.toInt).toList
    (for
      pass <- min to max
      passDigits = pass.toString.map(_.asDigit)
      if passDigits.zip(passDigits.tail).exists((fst, snd) => fst == snd)
      if passDigits.zip(passDigits.tail).forall((fst, snd) => fst <= snd)
    yield pass).size

  def solve2(input: String): Int =
    val min :: max :: Nil = "\\d+".r.findAllIn(input).map(_.toInt).toList
    (for
      pass <- min to max
      passDigits = pass.toString.map(_.asDigit)
      if passDigits.sliding(3).exists(_.distinct.size == 2)
      if passDigits.zip(passDigits.tail).forall((fst, snd) => fst <= snd)
    yield pass).size

  val input = "264360-746325"
