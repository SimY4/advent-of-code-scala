package aoc.y2019

object Day4
  private val pairsRegex = (0 to 9)
    .map(i => s"$i$i")
    .mkString("(", "|", ")").r

  private val dupesRegex = (0 to 9)
    .map(i => s"$i$i+")
    .mkString("(", "|", ")").r

  private def isAlwaysGrowing(pass: String) = 
    pass.toList.map(_.toInt).sliding(2)
      .find { l => l(0) > l(1) }
      .isEmpty

  def solve(input: String): Int = 
    val range = input.split("-").map(_.toInt)
    (for 
      pass <- range(0) to range(1)
      passStr = pass.toString
      if pairsRegex.findFirstIn(passStr).isDefined
      if isAlwaysGrowing(passStr)
    yield passStr)
      .size

  def solve2(input: String): Int =
    val range = input.split("-").map(_.toInt)
    (for 
      pass <- range(0) to range(1)
      passStr = pass.toString
      if !dupesRegex.findAllIn(passStr).filter(_.length == 2).isEmpty
      if isAlwaysGrowing(passStr)
    yield passStr)
      .size
  
  val input = "264360-746325"