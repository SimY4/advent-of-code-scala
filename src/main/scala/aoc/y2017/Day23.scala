package aoc.y2017

import scala.annotation.tailrec

object Day23:
  private enum Ins:
    case Set(reg: String, value: Long Either String)
    case Sub(reg: String, value: Long Either String)
    case Mul(reg: String, value: Long Either String)
    case Jnz(reg: Long Either String, value: Long Either String)

  import Ins.*

  private def parseLine(line: String): Ins =
    line match
      case s"set $r $x" => Set(r, x.toLongOption.toLeft(x))
      case s"sub $r $x" => Sub(r, x.toLongOption.toLeft(x))
      case s"mul $r $x" => Mul(r, x.toLongOption.toLeft(x))
      case s"jnz $r $x" => Jnz(r.toLongOption.toLeft(r), x.toLongOption.toLeft(x))

  @tailrec private def runProgram(
    program: Vector[Ins],
    state: Map[String, Long] = Map.empty.withDefaultValue(0L),
    cursor: Int = 0,
    count: Int = 0
  ): Int =
    program.lift(cursor) match
      case None => count
      case Some(Set(reg, value)) =>
        runProgram(program, state.updated(reg, value.fold(identity, state)), cursor + 1, count)
      case Some(Sub(reg, value)) =>
        runProgram(program, state.updated(reg, state(reg) - value.fold(identity, state)), cursor + 1, count)
      case Some(Mul(reg, value)) =>
        runProgram(program, state.updated(reg, state(reg) * value.fold(identity, state)), cursor + 1, count + 1)
      case Some(Jnz(reg, value)) =>
        runProgram(
          program,
          state,
          cursor + (if reg.fold(identity, state) != 0 then value.fold(_.toInt, state(_).toInt) else 1),
          count
        )

  def solve(input: String): Int =
    val program = input.linesIterator.map(parseLine).toVector

    runProgram(program)

  def solve2(input: String): Int =
    var b   = 57L * 100L + 100000L
    val c   = b + 17000L
    var h   = 0
    var end = false
    while !end do
      var f = 1
      var d = 2

      var loop = true
      while loop do
        f = if b % d == 0 then 0 else f
        d += 1
        loop = d != b

      if f == 0 then h += 1
      end = b == c
      b += 17L
    h

  val input = """set b 57
                |set c b
                |jnz a 2
                |jnz 1 5
                |mul b 100
                |sub b -100000
                |set c b
                |sub c -17000
                |set f 1
                |set d 2
                |set e 2
                |set g d
                |mul g e
                |sub g b
                |jnz g 2
                |set f 0
                |sub e -1
                |set g e
                |sub g b
                |jnz g -8
                |sub d -1
                |set g d
                |sub g b
                |jnz g -13
                |jnz f 2
                |sub h -1
                |set g b
                |sub g c
                |jnz g 2
                |jnz 1 3
                |sub b -17
                |jnz 1 -23""".stripMargin
