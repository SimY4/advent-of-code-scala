package aoc.y2022

object Day5:
  final private case class Move(qty: Int, from: Int, to: Int)

  private def parse(input: String): (List[List[String]], Vector[Move]) =
    val crates = input
      .substring(0, input.indexOf("\n\n"))
      .linesIterator
      .toVector

    (
      crates.init
        .foldRight(List.fill("\\d+".r.findAllIn(crates.last).size)(Nil: List[String])): (line, acc) =>
          line
            .sliding(3, 4)
            .zipWithIndex
            .foldLeft(acc):
              case (acc, (s"[$s]", i)) => acc.updated(i, s :: acc(i))
              case (acc, _)            => acc,
      input
        .substring(input.indexOf("\n\n") + 2)
        .linesIterator
        .map: line =>
          "\\d+".r.findAllIn(line).map(_.toInt).toList match
            case qty :: from :: to :: Nil => Move(qty, from - 1, to - 1)
        .toVector
    )

  def solve(input: String): String =
    val (crates, rearangements) = parse(input)
    rearangements
      .foldLeft(crates): (acc, move) =>
        val from = acc(move.from)
        val qty  = from.take(move.qty)
        acc.updated(move.from, from.drop(move.qty)).updated(move.to, qty.reverse ::: acc(move.to))
      .map(_.head)
      .mkString

  def solve2(input: String): String =
    val (crates, rearangements) = parse(input)
    rearangements
      .foldLeft(crates): (acc, move) =>
        val from = acc(move.from)
        val qty  = from.take(move.qty)
        acc.updated(move.from, from.drop(move.qty)).updated(move.to, qty ::: acc(move.to))
      .map(_.head)
      .mkString

  val input = """    [C]             [L]         [T]
                |    [V] [R] [M]     [T]         [B]
                |    [F] [G] [H] [Q] [Q]         [H]
                |    [W] [L] [P] [V] [M] [V]     [F]
                |    [P] [C] [W] [S] [Z] [B] [S] [P]
                |[G] [R] [M] [B] [F] [J] [S] [Z] [D]
                |[J] [L] [P] [F] [C] [H] [F] [J] [C]
                |[Z] [Q] [F] [L] [G] [W] [H] [F] [M]
                | 1   2   3   4   5   6   7   8   9 
                |
                |move 1 from 5 to 6
                |move 5 from 6 to 7
                |move 10 from 7 to 3
                |move 4 from 8 to 4
                |move 2 from 5 to 4
                |move 4 from 3 to 6
                |move 6 from 2 to 4
                |move 8 from 6 to 9
                |move 5 from 9 to 2
                |move 7 from 2 to 7
                |move 2 from 1 to 4
                |move 3 from 3 to 8
                |move 1 from 5 to 9
                |move 1 from 3 to 8
                |move 1 from 1 to 2
                |move 11 from 4 to 6
                |move 2 from 5 to 6
                |move 10 from 9 to 1
                |move 4 from 8 to 3
                |move 7 from 7 to 1
                |move 9 from 1 to 2
                |move 1 from 6 to 5
                |move 1 from 5 to 9
                |move 5 from 3 to 8
                |move 2 from 9 to 1
                |move 5 from 3 to 9
                |move 3 from 6 to 8
                |move 5 from 9 to 6
                |move 6 from 6 to 3
                |move 3 from 3 to 2
                |move 1 from 9 to 8
                |move 13 from 2 to 3
                |move 3 from 8 to 1
                |move 11 from 1 to 4
                |move 3 from 4 to 1
                |move 2 from 6 to 5
                |move 4 from 6 to 8
                |move 17 from 3 to 9
                |move 1 from 1 to 8
                |move 1 from 6 to 5
                |move 1 from 3 to 7
                |move 1 from 7 to 4
                |move 3 from 4 to 1
                |move 1 from 3 to 8
                |move 4 from 8 to 1
                |move 3 from 5 to 9
                |move 1 from 6 to 4
                |move 4 from 4 to 8
                |move 2 from 8 to 4
                |move 2 from 1 to 6
                |move 4 from 8 to 6
                |move 1 from 8 to 3
                |move 6 from 6 to 3
                |move 6 from 3 to 9
                |move 6 from 1 to 4
                |move 5 from 8 to 4
                |move 1 from 3 to 6
                |move 3 from 1 to 7
                |move 1 from 6 to 7
                |move 4 from 4 to 5
                |move 24 from 9 to 5
                |move 2 from 9 to 1
                |move 27 from 5 to 7
                |move 13 from 7 to 2
                |move 1 from 5 to 9
                |move 7 from 2 to 7
                |move 1 from 9 to 8
                |move 5 from 2 to 8
                |move 1 from 2 to 5
                |move 1 from 5 to 7
                |move 21 from 4 to 1
                |move 1 from 4 to 6
                |move 1 from 6 to 5
                |move 22 from 7 to 5
                |move 2 from 7 to 8
                |move 7 from 5 to 4
                |move 1 from 4 to 5
                |move 2 from 7 to 9
                |move 5 from 5 to 2
                |move 5 from 4 to 2
                |move 3 from 5 to 1
                |move 7 from 8 to 7
                |move 1 from 4 to 1
                |move 23 from 1 to 8
                |move 2 from 9 to 4
                |move 11 from 8 to 3
                |move 3 from 1 to 3
                |move 1 from 4 to 2
                |move 12 from 3 to 2
                |move 7 from 7 to 3
                |move 3 from 2 to 1
                |move 1 from 4 to 9
                |move 1 from 1 to 3
                |move 9 from 8 to 6
                |move 2 from 5 to 4
                |move 3 from 1 to 7
                |move 3 from 2 to 4
                |move 7 from 2 to 3
                |move 9 from 3 to 4
                |move 7 from 5 to 2
                |move 2 from 7 to 2
                |move 1 from 7 to 2
                |move 13 from 4 to 6
                |move 1 from 9 to 8
                |move 2 from 8 to 2
                |move 12 from 2 to 1
                |move 3 from 3 to 1
                |move 1 from 8 to 1
                |move 5 from 3 to 7
                |move 3 from 2 to 8
                |move 7 from 2 to 5
                |move 3 from 8 to 3
                |move 1 from 4 to 8
                |move 22 from 6 to 4
                |move 1 from 3 to 6
                |move 3 from 5 to 8
                |move 4 from 5 to 8
                |move 1 from 3 to 9
                |move 8 from 4 to 2
                |move 8 from 8 to 3
                |move 1 from 6 to 3
                |move 4 from 2 to 6
                |move 1 from 9 to 4
                |move 5 from 3 to 9
                |move 2 from 8 to 1
                |move 3 from 2 to 1
                |move 10 from 4 to 8
                |move 4 from 7 to 6
                |move 10 from 1 to 3
                |move 9 from 8 to 2
                |move 1 from 7 to 1
                |move 15 from 3 to 1
                |move 1 from 8 to 9
                |move 4 from 4 to 1
                |move 17 from 1 to 3
                |move 3 from 2 to 3
                |move 3 from 6 to 8
                |move 5 from 9 to 7
                |move 11 from 1 to 8
                |move 4 from 7 to 8
                |move 6 from 2 to 5
                |move 2 from 1 to 4
                |move 4 from 6 to 8
                |move 16 from 8 to 6
                |move 2 from 6 to 1
                |move 1 from 9 to 5
                |move 1 from 7 to 5
                |move 2 from 5 to 6
                |move 5 from 6 to 3
                |move 2 from 8 to 5
                |move 1 from 2 to 1
                |move 10 from 6 to 3
                |move 6 from 5 to 9
                |move 2 from 1 to 2
                |move 2 from 4 to 2
                |move 1 from 2 to 4
                |move 5 from 9 to 2
                |move 1 from 4 to 3
                |move 1 from 9 to 7
                |move 1 from 6 to 1
                |move 1 from 1 to 7
                |move 2 from 7 to 5
                |move 7 from 2 to 5
                |move 6 from 5 to 1
                |move 1 from 2 to 3
                |move 1 from 4 to 1
                |move 2 from 8 to 9
                |move 8 from 1 to 3
                |move 2 from 5 to 3
                |move 29 from 3 to 9
                |move 5 from 3 to 8
                |move 6 from 8 to 5
                |move 1 from 6 to 5
                |move 6 from 3 to 2
                |move 2 from 2 to 4
                |move 1 from 1 to 7
                |move 18 from 9 to 6
                |move 2 from 2 to 9
                |move 2 from 2 to 8
                |move 13 from 6 to 8
                |move 1 from 7 to 4
                |move 3 from 5 to 6
                |move 1 from 5 to 7
                |move 1 from 7 to 4
                |move 14 from 9 to 3
                |move 3 from 4 to 5
                |move 1 from 9 to 7
                |move 14 from 3 to 2
                |move 1 from 7 to 3
                |move 4 from 2 to 5
                |move 16 from 8 to 6
                |move 11 from 6 to 9
                |move 13 from 6 to 4
                |move 5 from 5 to 2
                |move 12 from 2 to 4
                |move 19 from 4 to 3
                |move 7 from 4 to 5
                |move 14 from 5 to 2
                |move 2 from 3 to 6
                |move 3 from 9 to 5
                |move 2 from 6 to 2
                |move 1 from 5 to 2
                |move 3 from 5 to 4
                |move 3 from 4 to 1
                |move 7 from 9 to 6
                |move 4 from 6 to 1
                |move 1 from 1 to 8
                |move 3 from 6 to 9
                |move 1 from 8 to 7
                |move 1 from 9 to 6
                |move 4 from 1 to 2
                |move 1 from 7 to 2
                |move 2 from 9 to 8
                |move 10 from 2 to 9
                |move 2 from 2 to 9
                |move 11 from 3 to 7
                |move 1 from 8 to 9
                |move 2 from 3 to 7
                |move 1 from 1 to 7
                |move 10 from 2 to 4
                |move 3 from 4 to 1
                |move 4 from 1 to 8
                |move 1 from 6 to 5
                |move 6 from 7 to 9
                |move 3 from 9 to 1
                |move 1 from 5 to 1
                |move 4 from 4 to 2
                |move 5 from 2 to 1
                |move 1 from 2 to 7
                |move 2 from 7 to 6
                |move 1 from 2 to 1
                |move 2 from 9 to 1
                |move 3 from 4 to 7
                |move 1 from 3 to 7
                |move 2 from 8 to 3
                |move 2 from 6 to 5
                |move 2 from 5 to 8
                |move 10 from 7 to 2
                |move 6 from 9 to 1
                |move 1 from 7 to 3
                |move 2 from 8 to 9
                |move 7 from 3 to 7
                |move 7 from 3 to 9
                |move 1 from 8 to 9
                |move 6 from 2 to 8
                |move 13 from 9 to 1
                |move 6 from 9 to 8
                |move 2 from 2 to 7
                |move 3 from 7 to 1
                |move 1 from 8 to 1
                |move 1 from 1 to 6
                |move 16 from 1 to 4
                |move 2 from 7 to 5
                |move 12 from 4 to 9
                |move 4 from 8 to 6
                |move 2 from 5 to 1
                |move 8 from 8 to 4
                |move 2 from 4 to 5
                |move 1 from 8 to 6
                |move 4 from 6 to 8
                |move 19 from 1 to 9
                |move 3 from 8 to 5
                |move 1 from 6 to 9
                |move 2 from 2 to 1
                |move 10 from 4 to 9
                |move 1 from 1 to 2
                |move 2 from 1 to 5
                |move 4 from 7 to 9
                |move 1 from 8 to 2
                |move 1 from 2 to 6
                |move 7 from 5 to 4
                |move 11 from 9 to 8
                |move 1 from 4 to 3
                |move 10 from 8 to 1
                |move 1 from 2 to 3
                |move 29 from 9 to 3
                |move 2 from 6 to 5
                |move 1 from 5 to 3
                |move 5 from 9 to 3
                |move 1 from 8 to 9
                |move 1 from 9 to 3
                |move 6 from 4 to 6
                |move 1 from 5 to 1
                |move 1 from 6 to 3
                |move 2 from 1 to 5
                |move 1 from 9 to 5
                |move 37 from 3 to 2
                |move 3 from 6 to 2
                |move 1 from 6 to 2
                |move 1 from 6 to 4
                |move 3 from 1 to 3
                |move 2 from 1 to 6
                |move 35 from 2 to 1
                |move 1 from 6 to 8
                |move 5 from 1 to 8
                |move 7 from 1 to 6
                |move 5 from 3 to 7
                |move 1 from 8 to 7
                |move 3 from 7 to 5
                |move 4 from 2 to 9
                |move 1 from 2 to 1
                |move 1 from 4 to 3
                |move 3 from 7 to 1
                |move 1 from 3 to 6
                |move 1 from 1 to 9
                |move 5 from 9 to 2
                |move 18 from 1 to 3
                |move 6 from 1 to 8
                |move 6 from 3 to 7
                |move 4 from 8 to 6
                |move 4 from 6 to 7
                |move 9 from 7 to 8
                |move 3 from 2 to 7
                |move 4 from 6 to 1
                |move 3 from 5 to 3
                |move 3 from 2 to 5
                |move 3 from 6 to 1
                |move 4 from 7 to 4
                |move 6 from 5 to 9
                |move 3 from 1 to 9
                |move 1 from 6 to 1
                |move 15 from 8 to 2
                |move 1 from 8 to 5
                |move 3 from 4 to 8
                |move 1 from 5 to 1
                |move 1 from 6 to 5
                |move 11 from 3 to 9
                |move 12 from 2 to 3
                |move 3 from 8 to 1
                |move 15 from 1 to 2
                |move 8 from 9 to 4
                |move 8 from 4 to 9
                |move 4 from 2 to 5
                |move 1 from 4 to 6
                |move 1 from 2 to 8
                |move 1 from 6 to 7
                |move 4 from 3 to 1
                |move 1 from 8 to 5
                |move 5 from 3 to 9
                |move 14 from 9 to 2
                |move 1 from 7 to 4
                |move 4 from 1 to 3
                |move 1 from 4 to 7
                |move 8 from 3 to 7
                |move 8 from 7 to 5
                |move 1 from 7 to 9
                |move 3 from 3 to 2
                |move 7 from 9 to 8
                |move 1 from 9 to 5
                |move 2 from 8 to 5
                |move 7 from 5 to 4
                |move 4 from 9 to 2
                |move 6 from 4 to 3
                |move 18 from 2 to 5
                |move 1 from 4 to 7
                |move 15 from 5 to 4
                |move 1 from 4 to 6
                |move 2 from 2 to 7
                |move 3 from 8 to 5
                |move 1 from 7 to 3
                |move 8 from 2 to 6
                |move 4 from 2 to 3
                |move 1 from 7 to 5
                |move 3 from 4 to 6
                |move 5 from 6 to 9
                |move 8 from 5 to 6
                |move 2 from 4 to 3
                |move 7 from 4 to 2
                |move 2 from 8 to 5
                |move 7 from 5 to 6
                |move 3 from 5 to 8
                |move 1 from 8 to 9
                |move 13 from 3 to 8
                |move 2 from 2 to 7
                |move 9 from 8 to 9
                |move 6 from 8 to 5
                |move 5 from 5 to 2
                |move 2 from 7 to 8
                |move 9 from 2 to 5
                |move 1 from 7 to 5
                |move 1 from 5 to 7
                |move 21 from 6 to 2
                |move 1 from 7 to 8
                |move 3 from 8 to 9
                |move 1 from 4 to 2
                |move 23 from 2 to 7
                |move 8 from 9 to 8
                |move 20 from 7 to 4
                |move 3 from 7 to 2
                |move 1 from 2 to 7
                |move 1 from 6 to 7
                |move 3 from 5 to 4
                |move 8 from 5 to 9
                |move 2 from 7 to 1
                |move 1 from 8 to 7
                |move 4 from 2 to 4
                |move 2 from 8 to 7
                |move 2 from 8 to 2
                |move 1 from 7 to 6
                |move 3 from 9 to 7
                |move 2 from 2 to 7
                |move 5 from 7 to 1
                |move 8 from 9 to 6
                |move 15 from 4 to 3
                |move 4 from 4 to 7
                |move 6 from 1 to 4
                |move 11 from 3 to 4
                |move 8 from 6 to 1
                |move 24 from 4 to 7
                |move 6 from 1 to 8
                |move 27 from 7 to 3
                |move 2 from 7 to 8
                |move 5 from 8 to 3
                |move 4 from 8 to 4
                |move 1 from 8 to 6
                |move 1 from 6 to 9
                |move 1 from 6 to 5
                |move 2 from 4 to 2
                |move 1 from 8 to 1
                |move 1 from 5 to 2
                |move 4 from 1 to 6
                |move 1 from 7 to 5
                |move 1 from 5 to 8
                |move 1 from 8 to 7
                |move 1 from 7 to 8
                |move 1 from 8 to 1
                |move 1 from 2 to 3
                |move 2 from 4 to 8
                |move 7 from 9 to 6
                |move 2 from 8 to 1
                |move 3 from 3 to 8
                |move 3 from 1 to 8
                |move 2 from 2 to 3
                |move 1 from 4 to 1
                |move 1 from 1 to 8
                |move 5 from 8 to 3
                |move 8 from 6 to 2
                |move 1 from 9 to 4
                |move 2 from 4 to 8
                |move 2 from 8 to 3
                |move 2 from 6 to 2
                |move 33 from 3 to 2
                |move 2 from 8 to 7
                |move 1 from 6 to 1
                |move 1 from 1 to 7
                |move 2 from 3 to 8
                |move 2 from 8 to 4
                |move 1 from 4 to 8
                |move 2 from 7 to 2
                |move 2 from 3 to 7
                |move 12 from 2 to 1
                |move 1 from 8 to 4
                |move 1 from 4 to 8
                |move 1 from 4 to 3
                |move 1 from 8 to 2
                |move 3 from 7 to 2
                |move 37 from 2 to 7
                |move 1 from 1 to 7
                |move 12 from 7 to 1
                |move 13 from 1 to 7
                |move 1 from 3 to 4
                |move 35 from 7 to 6
                |move 1 from 4 to 5
                |move 3 from 7 to 4
                |move 1 from 5 to 7
                |move 2 from 3 to 4
                |move 23 from 6 to 9
                |move 3 from 1 to 5
                |move 3 from 3 to 7
                |move 1 from 3 to 6
                |move 2 from 5 to 3
                |move 23 from 9 to 8
                |move 2 from 4 to 9
                |move 16 from 8 to 2
                |move 2 from 7 to 3
                |move 1 from 5 to 8
                |move 3 from 7 to 6
                |move 1 from 9 to 8
                |move 3 from 8 to 1
                |move 1 from 9 to 1
                |move 11 from 6 to 5
                |move 2 from 4 to 1
                |move 4 from 8 to 6
                |move 16 from 2 to 3
                |move 9 from 1 to 9
                |move 1 from 8 to 4
                |move 3 from 9 to 3
                |move 1 from 1 to 4
                |move 1 from 9 to 4
                |move 7 from 5 to 2
                |move 6 from 2 to 5
                |move 1 from 8 to 6
                |move 22 from 3 to 7
                |move 8 from 5 to 8
                |move 4 from 4 to 9
                |move 2 from 1 to 8
                |move 16 from 7 to 2
                |move 1 from 3 to 5
                |move 14 from 2 to 7
                |move 2 from 2 to 4
                |move 6 from 9 to 3""".stripMargin
