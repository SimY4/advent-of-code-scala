package aoc.y2015

import scala.annotation.tailrec

object Day23:
  private enum Ins:
    case Hlf(r: String)
    case Tpl(r: String)
    case Inc(r: String)
    case Jmp(offset: Int)
    case Jie(r: String, offset: Int)
    case Jio(r: String, offset: Int)

  import Ins.*

  private def parseLine(line: String): Ins =
    line match
      case s"hlf $r"          => Hlf(r)
      case s"tpl $r"          => Tpl(r)
      case s"inc $r"          => Inc(r)
      case s"jmp $offset"     => Jmp(offset.toInt)
      case s"jie $r, $offset" => Jie(r, offset.toInt)
      case s"jio $r, $offset" => Jio(r, offset.toInt)

  @tailrec private def runProgram(program: Vector[Ins], state: Map[String, Int], cursor: Int = 0): Map[String, Int] =
    program.lift(cursor) match
      case None                              => state
      case Some(Hlf(r: String))              => runProgram(program, state.updatedWith(r)(_.map(_ / 2)), cursor + 1)
      case Some(Tpl(r: String))              => runProgram(program, state.updatedWith(r)(_.map(_ * 3)), cursor + 1)
      case Some(Inc(r: String))              => runProgram(program, state.updatedWith(r)(_.map(_ + 1)), cursor + 1)
      case Some(Jmp(offset: Int))            => runProgram(program, state, cursor + offset)
      case Some(Jie(r: String, offset: Int)) =>
        runProgram(program, state, cursor + (if (state(r) & 1) == 0 then offset else 1))
      case Some(Jio(r: String, offset: Int)) =>
        runProgram(program, state, cursor + (if state(r) == 1 then offset else 1))

  def solve(input: String): Int =
    val program = input.linesIterator.map(parseLine).toVector

    runProgram(program, Map("a" -> 0, "b" -> 0))("b")

  def solve2(input: String): Int =
    val program = input.linesIterator.map(parseLine).toVector

    runProgram(program, Map("a" -> 1, "b" -> 0))("b")

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
