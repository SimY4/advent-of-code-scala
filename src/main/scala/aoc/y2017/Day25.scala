package aoc.y2017

object Day25:
  final private case class Rule(write: Int, move: Int, next: String)
  final private case class State(if0: Rule, if1: Rule)

  private def parseInput(input: String): (String, Int, Map[String, State]) =
    val lines        = input.linesIterator.toVector
    val initialState = lines(0) match
      case s"Begin in state $state." => state
    val steps = lines(1) match
      case s"Perform a diagnostic checksum after $steps steps." => steps.toInt
    val states = lines
      .drop(3)
      .grouped(10)
      .map: state =>
        val name = state(0) match
          case s"In state $state:" => state

        def mapLine(rule: Rule, line: String): Rule =
          line match
            case s"    - Write the value $value."    => rule.copy(write = value.toInt)
            case "    - Move one slot to the right." => rule.copy(move = 1)
            case "    - Move one slot to the left."  => rule.copy(move = -1)
            case s"    - Continue with state $next." => rule.copy(next = next)

        val if0 = (2 to 4).map(state(_)).foldLeft(Rule(0, 0, ""))(mapLine)
        val if1 = (6 to 8).map(state(_)).foldLeft(Rule(0, 0, ""))(mapLine)
        name -> State(if0, if1)
      .toMap
    (initialState, steps, states)

  def solve(input: String): Int =
    val (start, steps, states) = parseInput(input)

    LazyList
      .iterate((Vector.fill(20000)(0), 10000, start)): (tape, cursor, st) =>
        val current   = tape(cursor)
        val state     = states(st)
        val rule      = if current == 0 then state.if0 else state.if1
        val newTape   = tape.updated(cursor, rule.write)
        val newCursor = cursor + rule.move
        (newTape, newCursor, rule.next)
      .drop(steps)
      .head(0)
      .count(_ == 1)

  val input = """Begin in state A.
                |Perform a diagnostic checksum after 12459852 steps.
                |
                |In state A:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state B.
                |  If the current value is 1:
                |    - Write the value 1.
                |    - Move one slot to the left.
                |    - Continue with state E.
                |
                |In state B:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state C.
                |  If the current value is 1:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state F.
                |
                |In state C:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the left.
                |    - Continue with state D.
                |  If the current value is 1:
                |    - Write the value 0.
                |    - Move one slot to the right.
                |    - Continue with state B.
                |
                |In state D:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state E.
                |  If the current value is 1:
                |    - Write the value 0.
                |    - Move one slot to the left.
                |    - Continue with state C.
                |
                |In state E:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the left.
                |    - Continue with state A.
                |  If the current value is 1:
                |    - Write the value 0.
                |    - Move one slot to the right.
                |    - Continue with state D.
                |
                |In state F:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state A.
                |  If the current value is 1:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state C.
                |""".stripMargin
