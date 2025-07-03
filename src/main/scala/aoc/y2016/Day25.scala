package aoc
package y2016

import scala.annotation.tailrec

object Day25:
  private enum Code:
    case Cpy(value: Int Either String, reg: String)
    case Inc(reg: String)
    case Dec(reg: String)
    case Jnz(value: Int Either String, nOrReg: Int Either String)
    case Tgl(reg: String)
    case Out(value: Int Either String)

  import Code.*

  private def parseLine(line: String): Code = line match
    case s"cpy ${value} ${reg}"    => Cpy(value.toIntOption.toLeft(value), reg)
    case s"inc ${reg}"             => Inc(reg)
    case s"dec ${reg}"             => Dec(reg)
    case s"jnz ${value} ${nOrReg}" => Jnz(value.toIntOption.toLeft(value), nOrReg.toIntOption.toLeft(nOrReg))
    case s"tgl ${reg}"             => Tgl(reg)
    case s"out ${value}"           => Out(value.toIntOption.toLeft(value))

  @tailrec private def runProgram(
    instructions: Vector[Code],
    state: Map[String, Int],
    pos: Int = 0,
    out: List[Int] = Nil
  ): List[Int] =
    if out.size > 10 then out
    else
      instructions.lift(pos) match
        case None                  => out
        case Some(Cpy(value, reg)) =>
          runProgram(instructions, state.updated(reg, value.fold(identity, state)), pos + 1, out)
        case Some(Inc(reg))           => runProgram(instructions, state.updated(reg, state(reg) + 1), pos + 1, out)
        case Some(Dec(reg))           => runProgram(instructions, state.updated(reg, state(reg) - 1), pos + 1, out)
        case Some(Jnz(value, nOrReg)) =>
          runProgram(
            instructions,
            state,
            if value.fold(identity, state) == 0 then pos + 1 else pos + nOrReg.fold(identity, state),
            out
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
              pos + 1,
              out
            )
          else runProgram(instructions, state, pos + 1, out)
        case Some(Out(value)) => runProgram(instructions, state, pos + 1, value.fold(identity, state) :: out)

  def solve(input: String): Option[Int] =
    (0 to 10000)
      .find(a =>
        runProgram(input.linesIterator.map(parseLine).toVector, Map("a" -> a)).zipWithIndex.forall((out, idx) =>
          out == idx % 2
        )
      )

  val input = """cpy a d
                |cpy 7 c
                |cpy 365 b
                |inc d
                |dec b
                |jnz b -2
                |dec c
                |jnz c -5
                |cpy d a
                |jnz 0 0
                |cpy a b
                |cpy 0 a
                |cpy 2 c
                |jnz b 2
                |jnz 1 6
                |dec b
                |dec c
                |jnz c -4
                |inc a
                |jnz 1 -7
                |cpy 2 b
                |jnz c 2
                |jnz 1 4
                |dec b
                |dec c
                |jnz 1 -4
                |jnz 0 0
                |out b
                |jnz a -19
                |jnz 1 -21""".stripMargin
