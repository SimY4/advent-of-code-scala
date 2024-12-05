package aoc.y2016

object Day21:
  private enum Operation:
    case SwapPos(x: Int, y: Int)
    case SwapLetter(x: Char, y: Char)
    case RotatePos(right: Boolean, x: Int)
    case RotateLetter(x: Char)
    case Reverse(x: Int, y: Int)
    case Move(x: Int, y: Int)
  import Operation.*

  private def parseLine(line: String): Operation = line match
    case s"swap position $x with position $y"     => SwapPos(x.toInt, y.toInt)
    case s"swap letter $x with letter $y"         => SwapLetter(x.head, y.head)
    case s"rotate left $x step$_"                 => RotatePos(false, x.toInt)
    case s"rotate right $x step$_"                => RotatePos(true, x.toInt)
    case s"rotate based on position of letter $x" => RotateLetter(x.head)
    case s"reverse positions $x through $y"       => Reverse(x.toInt, y.toInt)
    case s"move position $x to position $y"       => Move(x.toInt, y.toInt)

  def solve(input: String): String =
    input.linesIterator
      .map(parseLine)
      .foldLeft("abcdefgh"):
        case (pwd, SwapPos(x, y))       => pwd.updated(y, pwd(x)).updated(x, pwd(y))
        case (pwd, SwapLetter(x, y))    => pwd.updated(pwd.indexOf(y), x).updated(pwd.indexOf(x), y)
        case (pwd, RotatePos(false, x)) => pwd.indices.map(i => pwd((i + x) % pwd.length)).mkString
        case (pwd, RotatePos(true, x))  => pwd.indices.map(i => pwd.reverse((i + x) % pwd.length)).mkString.reverse
        case (pwd, RotateLetter(x)) =>
          val times = 1 + pwd.indexOf(x) + (if pwd.indexOf(x) >= 4 then 1 else 0)
          pwd.indices.map(i => pwd.reverse((i + times) % pwd.length)).mkString.reverse
        case (pwd, Reverse(x, y)) => pwd.patch(x, pwd.substring(x, y + 1).reverse, y - x + 1)
        case (pwd, Move(x, y))    => pwd.patch(x, Nil, 1).patch(y, Seq(pwd(x)), 0)

  def solve2(input: String): String =
    input.linesIterator
      .map(parseLine)
      .toList
      .reverse
      .foldLeft("fbgdceah"):
        case (pwd, SwapPos(x, y))       => pwd.updated(x, pwd(y)).updated(y, pwd(x))
        case (pwd, SwapLetter(x, y))    => pwd.updated(pwd.indexOf(x), y).updated(pwd.indexOf(y), x)
        case (pwd, RotatePos(false, x)) => pwd.indices.map(i => pwd.reverse((i + x) % pwd.length)).mkString.reverse
        case (pwd, RotatePos(true, x))  => pwd.indices.map(i => pwd((i + x) % pwd.length)).mkString
        case (pwd, RotateLetter(x)) =>
          val i = pwd.indexOf(x)
          val times = i match
            case 0 | 1 => 1
            case 2     => 6
            case 3     => 2
            case 4     => 7
            case 5     => 3
            case 6     => 0
            case 7     => 4
          pwd.indices.map(i => pwd((i + times) % pwd.length)).mkString
        case (pwd, Reverse(x, y)) => pwd.patch(x, pwd.substring(x, y + 1).reverse, y - x + 1)
        case (pwd, Move(x, y))    => pwd.patch(y, Nil, 1).patch(x, Seq(pwd(y)), 0)

  val input = """rotate right 3 steps
                |swap letter b with letter a
                |move position 3 to position 4
                |swap position 0 with position 7
                |swap letter f with letter h
                |rotate based on position of letter f
                |rotate based on position of letter b
                |swap position 3 with position 0
                |swap position 6 with position 1
                |move position 4 to position 0
                |rotate based on position of letter d
                |swap letter d with letter h
                |reverse positions 5 through 6
                |rotate based on position of letter h
                |reverse positions 4 through 5
                |move position 3 to position 6
                |rotate based on position of letter e
                |rotate based on position of letter c
                |rotate right 2 steps
                |reverse positions 5 through 6
                |rotate right 3 steps
                |rotate based on position of letter b
                |rotate right 5 steps
                |swap position 5 with position 6
                |move position 6 to position 4
                |rotate left 0 steps
                |swap position 3 with position 5
                |move position 4 to position 7
                |reverse positions 0 through 7
                |rotate left 4 steps
                |rotate based on position of letter d
                |rotate left 3 steps
                |swap position 0 with position 7
                |rotate based on position of letter e
                |swap letter e with letter a
                |rotate based on position of letter c
                |swap position 3 with position 2
                |rotate based on position of letter d
                |reverse positions 2 through 4
                |rotate based on position of letter g
                |move position 3 to position 0
                |move position 3 to position 5
                |swap letter b with letter d
                |reverse positions 1 through 5
                |reverse positions 0 through 1
                |rotate based on position of letter a
                |reverse positions 2 through 5
                |swap position 1 with position 6
                |swap letter f with letter e
                |swap position 5 with position 1
                |rotate based on position of letter a
                |move position 1 to position 6
                |swap letter e with letter d
                |reverse positions 4 through 7
                |swap position 7 with position 5
                |swap letter c with letter g
                |swap letter e with letter g
                |rotate left 4 steps
                |swap letter c with letter a
                |rotate left 0 steps
                |swap position 0 with position 1
                |reverse positions 1 through 4
                |rotate based on position of letter d
                |swap position 4 with position 2
                |rotate right 0 steps
                |swap position 1 with position 0
                |swap letter c with letter a
                |swap position 7 with position 3
                |swap letter a with letter f
                |reverse positions 3 through 7
                |rotate right 1 step
                |swap letter h with letter c
                |move position 1 to position 3
                |swap position 4 with position 2
                |rotate based on position of letter b
                |reverse positions 5 through 6
                |move position 5 to position 3
                |swap letter b with letter g
                |rotate right 6 steps
                |reverse positions 6 through 7
                |swap position 2 with position 5
                |rotate based on position of letter e
                |swap position 1 with position 7
                |swap position 1 with position 5
                |reverse positions 2 through 7
                |reverse positions 5 through 7
                |rotate left 3 steps
                |rotate based on position of letter b
                |rotate left 3 steps
                |swap letter e with letter c
                |rotate based on position of letter a
                |swap letter f with letter a
                |swap position 0 with position 6
                |swap position 4 with position 7
                |reverse positions 0 through 5
                |reverse positions 3 through 5
                |swap letter d with letter e
                |move position 0 to position 7
                |move position 1 to position 3
                |reverse positions 4 through 7""".stripMargin
