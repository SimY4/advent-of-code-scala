package aoc.y2019

import scala.annotation.tailrec

object Day2 {
  @tailrec private def runProgram(pointer: Int, opCodes: List[Int]): Int = opCodes.drop(pointer) match {
    case 1 :: x :: y :: z :: _ => runProgram(pointer + 4, opCodes.updated(z, opCodes(x) + opCodes(y)))
    case 2 :: x :: y :: z :: _ => runProgram(pointer + 4, opCodes.updated(z, opCodes(x) * opCodes(y)))
    case 99 :: _ => opCodes(0)
    case _ => opCodes(0)
  }

  def solve(input: String): Int = {
    val opCodes = input.split(",").map(_.toInt).toList
    runProgram(0, opCodes.updated(1, 12).updated(2, 2))
  }
    

  def solve2(input: String): Option[(Int, Int)] = {
    val opCodes = input.split(",").map(_.toInt).toList
    (for
      noun <- 0 to 99
      verb <- 0 to 99
      if runProgram(0, opCodes.updated(1, noun).updated(2, verb)) == 19690720
    yield (noun, verb))
      .headOption
  }
  
  val input = """1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,6,23,27,1,6,27,31,2,31,9,35,1,35,6,39,1,10,39,43,2,9,43,47,1,5,47,51,2,51,6,55,1,5,55,59,2,13,59,63,1,63,5,67,2,67,13,71,1,71,9,75,1,75,6,79,2,79,6,83,1,83,5,87,2,87,9,91,2,9,91,95,1,5,95,99,2,99,13,103,1,103,5,107,1,2,107,111,1,111,5,0,99,2,14,0,0""".stripMargin
}