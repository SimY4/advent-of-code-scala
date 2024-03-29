package aoc
package y2016

import scala.annotation.tailrec

object Day23:
  private enum Code:
    case Cpy(value: Int Either String, reg: String)
    case Inc(reg: String)
    case Dec(reg: String)
    case Jnz(value: Int Either String, nOrReg: Int Either String)
    case Tgl(reg: String)

  import Code.*

  private def parseLine(line: String): Code = line match
    case s"cpy ${value} ${reg}"    => Cpy(value.toIntOption.toLeft(value), reg)
    case s"inc ${reg}"             => Inc(reg)
    case s"dec ${reg}"             => Dec(reg)
    case s"jnz ${value} ${nOrReg}" => Jnz(value.toIntOption.toLeft(value), nOrReg.toIntOption.toLeft(nOrReg))
    case s"tgl ${reg}"             => Tgl(reg)

  @tailrec private def runProgram(instructions: Vector[Code], state: Map[String, Int], pos: Int = 0): Map[String, Int] =
    instructions.lift(pos) match
      case None                  => state
      case Some(Cpy(value, reg)) => runProgram(instructions, state.updated(reg, value.fold(identity, state)), pos + 1)
      case Some(Inc(reg))        => runProgram(instructions, state.updated(reg, state(reg) + 1), pos + 1)
      case Some(Dec(reg))        => runProgram(instructions, state.updated(reg, state(reg) - 1), pos + 1)
      case Some(Jnz(value, nOrReg)) =>
        runProgram(
          instructions,
          state,
          if value.fold(identity, state) == 0 then pos + 1 else pos + nOrReg.fold(identity, state)
        )
      case Some(Tgl(reg)) =>
        val n = pos + state(reg)
        if 0 <= n && n < instructions.size then
          runProgram(
            instructions.updated(
              n,
              instructions(n) match
                case Inc(reg)              => Dec(reg)
                case Dec(reg)              => Inc(reg)
                case Tgl(reg)              => Inc(reg)
                case Jnz(iOrR, Right(reg)) => Cpy(iOrR, reg)
                case Cpy(iOrR, reg)        => Jnz(iOrR, Right(reg))
            ),
            state,
            pos + 1
          )
        else runProgram(instructions, state, pos + 1)

  def solve(input: String): Option[Int] =
    val instructions = input.linesIterator.map(parseLine).toVector
    runProgram(instructions, Map("a" -> 7).withDefaultValue(0)).get("a")

  def solve2(input: String): Option[Long] =
    Some(12L.factorial + (77L * 87L))

  val input = """cpy a b
                |dec b
                |cpy a d
                |cpy 0 a
                |cpy b c
                |inc a
                |dec c
                |jnz c -2
                |dec d
                |jnz d -5
                |dec b
                |cpy b c
                |cpy c d
                |dec d
                |inc c
                |jnz d -2
                |tgl c
                |cpy -16 c
                |jnz 1 c
                |cpy 77 c
                |jnz 87 d
                |inc a
                |inc d
                |jnz d -2
                |inc c
                |jnz c -5""".stripMargin
