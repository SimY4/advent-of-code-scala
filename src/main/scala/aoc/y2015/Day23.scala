package aoc.y2015

import scala.annotation.tailrec

object Day23:
  private enum Instruction:
    case hlf(r: String)
    case tpl(r: String)
    case inc(r: String)
    case jmp(offset: Int)
    case jie(r: String, offset: Int)
    case jio(r: String, offset: Int)

  import Instruction.*

  private def parseLine(line: String): Instruction =
    line match
      case s"hlf $r"          => hlf(r)
      case s"tpl $r"          => tpl(r)
      case s"inc $r"          => inc(r)
      case s"jmp $offset"     => jmp(offset.toInt)
      case s"jie $r, $offset" => jie(r, offset.toInt)
      case s"jio $r, $offset" => jio(r, offset.toInt)

  @tailrec private def runProgram(program: List[Instruction], state: Map[String, Int], cursor: Int): Map[String, Int] =
    if cursor >= program.size then state
    else
      program(cursor) match
        case hlf(r: String)   => runProgram(program, state.updatedWith(r)(_.map(_ / 2)), cursor + 1)
        case tpl(r: String)   => runProgram(program, state.updatedWith(r)(_.map(_ * 3)), cursor + 1)
        case inc(r: String)   => runProgram(program, state.updatedWith(r)(_.map(_ + 1)), cursor + 1)
        case jmp(offset: Int) => runProgram(program, state, cursor + offset)
        case jie(r: String, offset: Int) =>
          runProgram(program, state, cursor + (if (state(r) & 1) == 0 then offset else 1))
        case jio(r: String, offset: Int) => runProgram(program, state, cursor + (if state(r) == 1 then offset else 1))

  def solve(input: String): Int =
    val program = input.linesIterator
      .map(parseLine)
      .toList

    runProgram(program, Map("a" -> 0, "b" -> 0), 0)("b")

  def solve2(input: String): Int =
    val program = input.linesIterator
      .map(parseLine)
      .toList

    runProgram(program, Map("a" -> 1, "b" -> 0), 0)("b")

  val input = """jio a, +18
                |inc a
                |tpl a
                |inc a
                |tpl a
                |tpl a
                |tpl a
                |inc a
                |tpl a
                |inc a
                |tpl a
                |inc a
                |inc a
                |tpl a
                |tpl a
                |tpl a
                |inc a
                |jmp +22
                |tpl a
                |inc a
                |tpl a
                |inc a
                |inc a
                |tpl a
                |inc a
                |tpl a
                |inc a
                |inc a
                |tpl a
                |tpl a
                |inc a
                |inc a
                |tpl a
                |inc a
                |inc a
                |tpl a
                |inc a
                |inc a
                |tpl a
                |jio a, +8
                |inc b
                |jie a, +4
                |tpl a
                |inc a
                |jmp +2
                |hlf a
                |jmp -7""".stripMargin
