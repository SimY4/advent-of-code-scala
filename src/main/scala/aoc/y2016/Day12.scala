package aoc.y2016

import scala.annotation.tailrec

object Day12:
  private enum Code:
    case Cpy(value: Int Either String, reg: String)
    case Inc(reg: String)
    case Dec(reg: String)
    case Jnz(value: Int Either String, n: Int)

  import Code.*

  private def parseLine(line: String): Code = line match
    case s"cpy ${value} ${reg}" => Cpy(value.toIntOption.toLeft(value), reg)
    case s"inc ${reg}"          => Inc(reg)
    case s"dec ${reg}"          => Dec(reg)
    case s"jnz ${value} ${n}"   => Jnz(value.toIntOption.toLeft(value), n.toInt)

  private def runProgram(instructions: List[Code], state: Map[String, Int], pos: Int = 0): Map[String, Int] =
    @tailrec def go(state: Map[String, Int], pos: Int): Map[String, Int] =
      instructions.lift(pos) match
        case None                     => state
        case Some(Cpy(Left(i), reg))  => go(state.updated(reg, i), pos + 1)
        case Some(Cpy(Right(r), reg)) => go(state.updated(reg, state(r)), pos + 1)
        case Some(Inc(reg))           => go(state.updated(reg, state(reg) + 1), pos + 1)
        case Some(Dec(reg))           => go(state.updated(reg, state(reg) - 1), pos + 1)
        case Some(Jnz(Left(i), n))    => go(state, if i == 0 then pos + 1 else pos + n)
        case Some(Jnz(Right(reg), n)) => go(state, if state(reg) == 0 then pos + 1 else pos + n)

    go(state, pos)

  def solve(input: String): Option[Int] =
    val instructions = input.linesIterator.map(parseLine).toList
    runProgram(instructions, Map.empty.withDefaultValue(0)).get("a")

  def solve2(input: String): Option[Int] =
    val instructions = input.linesIterator.map(parseLine).toList
    runProgram(instructions, Map("c" -> 1).withDefaultValue(0)).get("a")

  val input = """cpy 1 a
                |cpy 1 b
                |cpy 26 d
                |jnz c 2
                |jnz 1 5
                |cpy 7 c
                |inc d
                |dec c
                |jnz c -2
                |cpy a c
                |inc a
                |dec b
                |jnz b -2
                |cpy c b
                |dec d
                |jnz d -6
                |cpy 17 c
                |cpy 18 d
                |inc a
                |dec d
                |jnz d -2
                |dec c
                |jnz c -5""".stripMargin
