package aoc.y2017

object Day13:
  def solve(input: String): Int =
    val config = input.linesIterator
      .map: line =>
        "\\d+".r.findAllIn(line).map(_.toInt).toList match
          case i :: n :: _ => i -> n
      .toMap

    LazyList
      .iterate(config.keys.map(_ -> (0 -> 1)).toMap):
        _.map:
          case (i, (pos, inc)) =>
            val n       = config(i)
            val nextPos = pos + inc
            if nextPos < 0 || n <= nextPos then i -> (pos + -inc, -inc)
            else i                                -> (nextPos, inc)
      .take(config.keys.max + 1)
      .zipWithIndex
      .collect:
        case (state, i) if state.getOrElse(i, (-1, 0))(0) == 0 => i * config(i)
      .sum

  def solve2(input: String): Int =
    val config = input.linesIterator
      .map: line =>
        "\\d+".r.findAllIn(line).map(_.toInt).toList match
          case i :: n :: _ => i -> n
      .toMap

    LazyList
      .iterate(config.keys.map(_ -> (0 -> 1)).toMap):
        _.map:
          case (i, (pos, inc)) =>
            val n       = config(i)
            val nextPos = pos + inc
            if nextPos < 0 || n <= nextPos then i -> (pos + -inc, -inc)
            else i                                -> (nextPos, inc)
      .tails
      .indexWhere:
        _.take(config.keys.max + 1).zipWithIndex
          .forall(_.getOrElse(_, (-1, 0))(0) != 0)

  val input = """0: 3
                |1: 2
                |2: 4
                |4: 6
                |6: 5
                |8: 6
                |10: 6
                |12: 4
                |14: 8
                |16: 8
                |18: 9
                |20: 8
                |22: 6
                |24: 14
                |26: 12
                |28: 10
                |30: 12
                |32: 8
                |34: 10
                |36: 8
                |38: 8
                |40: 12
                |42: 12
                |44: 12
                |46: 12
                |48: 14
                |52: 14
                |54: 12
                |56: 12
                |58: 12
                |60: 12
                |62: 14
                |64: 14
                |66: 14
                |68: 14
                |70: 14
                |72: 14
                |80: 18
                |82: 14
                |84: 20
                |86: 14
                |90: 17
                |96: 20
                |98: 24""".stripMargin
