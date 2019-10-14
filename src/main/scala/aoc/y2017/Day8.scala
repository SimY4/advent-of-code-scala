package aoc
package y2017

object Day8 {
  import day8.InstructionsParser

  sealed trait Op
  case object Inc extends Op
  case object Dec extends Op

  sealed trait Comp
  case object Gt extends Comp
  case object Ge extends Comp
  case object Lt extends Comp
  case object Le extends Comp
  case object Eq extends Comp
  case object Ne extends Comp

  case class Command(command: Op, n: Int)
  case class Condition(register: String, comp: Comp, n: Int)

  case class Instruction(register: String, command: Command, condition: Condition)

  type CurrentAndMax = (Int, Int)

  private val input = """b inc 5 if a > 1
                        |a inc 1 if b < 5
                        |c dec -10 if a >= 1
                        |c inc -20 if c == 10""".stripMargin

  def eval(instructions: Seq[Instruction]): Map[String, CurrentAndMax] = {
    def evalCondition(registers: Map[String, CurrentAndMax], condition: Condition): Boolean = {
      val registerVal = registers.getOrElse(condition.register, 0 -> 0)._1
      condition.comp match {
        case Gt => registerVal > condition.n
        case Ge => registerVal >= condition.n
        case Lt => registerVal < condition.n
        case Le => registerVal <= condition.n
        case Eq => registerVal == condition.n
        case Ne => registerVal != condition.n
      }
    }

    instructions.foldLeft(Map.empty[String, CurrentAndMax]) { (registers, instruction) =>
      if (evalCondition(registers, instruction.condition)) {
        val registerVal = registers.getOrElse(instruction.register, 0 -> 0)
        val newCurrent = instruction.command match {
          case Command(Inc, n) => registerVal._1 + n
          case Command(Dec, n) => registerVal._1 - n
        }
        if (registerVal._2 >= newCurrent)
          registers.updated(instruction.register, (newCurrent, registerVal._2))
        else
          registers.updated(instruction.register, (newCurrent, newCurrent))
      } else
        registers
    }
  }

  println(for {
    instructions <- new InstructionsParser(input).Input.run()
    res          = eval(instructions)
    _            = println(res)
    max          = res.values.maxBy(_._2)
  } yield max)

}

package day8 {
  import org.parboiled2._

  class InstructionsParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Seq[Day8.Instruction]] = rule { Instructions ~ EOI }

    private def Instructions = rule { oneOrMore(Instruction).separatedBy(NewLine) }

    private def Instruction = rule { Register ~ ' ' ~ Command ~ ' ' ~ Condition ~> Day8.Instruction }

    private def Condition = rule { "if " ~ Register ~ Comp ~ Integer ~> Day8.Condition }

    private def Comp = rule {
      " > " ~ push(Day8.Gt) | " >= " ~ push(Day8.Ge) | " < " ~ push(Day8.Lt) | " <= " ~ push(Day8.Le) | " == " ~ push(
        Day8.Eq
      ) | " != " ~ push(Day8.Ne)
    }

    private def Register = rule { capture(oneOrMore(CharPredicate.Alpha)) }

    private def Command = rule { Op ~ ' ' ~ Integer ~> Day8.Command }

    private def Op = rule { "inc" ~ push(Day8.Inc) | "dec" ~ push(Day8.Dec) }

    private def Integer = rule {
      ('-' ~ UnsignedInteger ~> { i =>
        -i
      }) | UnsignedInteger
    }

    private def UnsignedInteger = rule { capture(oneOrMore(CharPredicate.Digit)) ~> (_.toInt) }

    private def NewLine = rule { optional('\r') ~ '\n' }

  }

}
