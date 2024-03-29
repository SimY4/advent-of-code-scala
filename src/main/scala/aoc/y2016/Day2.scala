package aoc
package y2016

object Day2:
  private val field1 = Array(
    Array(1, 2, 3),
    Array(4, 5, 6),
    Array(7, 8, 9)
  )

  def solve(input: String): String =
    input.linesIterator
      .scanLeft(Coord(1L, 1L)): (ccord, instructions) =>
        instructions.foldLeft(ccord): (coord, instruction) =>
          instruction match
            case 'U' if coord.y > 0L => coord.copy(y = coord.y - 1L)
            case 'R' if coord.x < 2L => coord.copy(x = coord.x + 1L)
            case 'D' if coord.y < 2L => coord.copy(y = coord.y + 1L)
            case 'L' if coord.x > 0L => coord.copy(x = coord.x - 1L)
            case _                   => coord
      .drop(1)
      .map(field1(_))
      .mkString

  private val field2 = Array(
    Array('_', '_', '1', '_', '_'),
    Array('_', '2', '3', '4', '_'),
    Array('5', '6', '7', '8', '9'),
    Array('_', 'A', 'B', 'C', '_'),
    Array('_', '_', 'D', '_', '_')
  )

  def solve2(input: String): String =
    input.linesIterator
      .scanLeft(Coord(0L, 2L)): (ccord, instructions) =>
        instructions.foldLeft(ccord): (coord, instruction) =>
          val Coord(x, y) = instruction match
            case 'U' if coord.y > 0L => coord.copy(y = coord.y - 1L)
            case 'R' if coord.x < 4L => coord.copy(x = coord.x + 1L)
            case 'D' if coord.y < 4L => coord.copy(y = coord.y + 1L)
            case 'L' if coord.x > 0L => coord.copy(x = coord.x - 1L)
            case _                   => coord
          if '_' == field2(y.toInt)(x.toInt) then coord else Coord(x, y)
      .drop(1)
      .map(field2(_))
      .mkString

  val input =
    """LRULLRLDUUUDUDDDRLUDRDLDDLUUDLDDLRDRLDRLLURRULURLDRLDUDURLURRULLDDDUDDRRRDLRRDDLDURDULLRDLLLDRDLLDULDUDLLDLDRUDLLDLDDRRRDRLUDRDDLUDRRDUDUDLLDDUUDLRDUDRRUDUDRULRULUDRUUDLDLULLRLDLDDRULLRLLLULUULDURURLUUULDURLDDDURRUUDURDDDULDLURLRDRURDRUDRLLDLDRUURLLLRDRURUDLRLUDULLDDURLRURDLRDUUURRLULRRLDDULUUURLRRRLLLLLURDDRUULUDRRRUDDLLULRRUULDRDDULRLDDDRRUULUDRLRUDURUUULDLDULUUDURLLLRRDDRDLURDDDLDDDLRDRLDDURLRLLRUDRRLLDDDDDURDURRDDULDULLRULDRUURDRRDUDDUDDDDRRDULDUURDRUDRLDULRULURLLRRDRDRDLUUDRRLRLDULDDLUUUUUURRLRRRULLDDDRLRDRRRRRRRDUUDLLUDURUDDLURRUDL
      |UDUUURRLRLLDDRRDRRRLDDDLURURLLUDDRLUUDRRRDURRLLRURDLLRRDUUDDDDRDRURRLLLLURDLRRRULLLDLLLUDDLDRRRDLDUUDDRDUDDUURDDLULULDURDURDRUULURURRURDUURUDRRUDRLLLLRRDLLDRDDRLLURDDDUDUDUDRUURDDRUURDLRUUDDRDUURUDDLLUURDLUDRUUDRRDLLUUURDULUULDUUDLLULUUDLUDRUUDUUURLDDDRLRURDDULLRDRULULUDLUUDDDUUDLDUUDRULLDUURDDRUDURULDRDDLRUULRRRDLDLRDULRDDRLLRRLURDLDRUDLRLUDLRLDLDURRUULRLUURDULDRRULLRULRDLLDLDUDRUDDUDLDDURDDDRDLUDRULRUULLRURLDDDRDLRRDRULURULDULRDLDULDURDRDRDRDURDRLUURLRDDLDDRLDDRURLLLURURDULDUDDLLUURDUUUDRUDDRDLDRLRLDURRULDULUUDDLRULDLRRRRDLLDRUUDRLLDLUDUULRDRDLRUUDLRRDDLUULDUULRUDRURLDDDURLRRULURR
      |LDURLLLRLLLUURLLULDLRLLDLURULRULRDUDLDDUDRLRRDLULLDDULUUULDRLDURURLURLDLRUDULLLULDUURLLRDLUULRULLLULRDRULUDLUUULDDURLUDDUDDRDLDRDRUDLUURDDLULDUULURLUULRDRDLURUDRUDLDRLUUUUULUDUDRRURUDRULDLDRDRLRURUUDRDLULLUDLLRUUDUUDUDLLRRRLDUDDDRDUDLDLLULRDURULLLUDLLRUDDUUDRLDUULLDLUUDUULURURLLULDUULLDLUDUURLURDLUULRRLLRUDRDLLLRRRLDDLUULUURLLDRDLUUULLDUDLLLLURDULLRUDUUULLDLRLDRLLULDUDUDRULLRRLULURUURLRLURRLRRRDDRLUDULURUDRRDLUDDRRDRUDRUDLDDRLRDRRLDDRLLDDDULDLRLDURRRRRULRULLUUULUUUDRRDRDRLLURRRRUULUDDUDDDLDURDRLDLLLLLRDUDLRDRUULU
      |URURRUUULLLLUURDULULLDLLULRUURRDRRLUULRDDRUDRRDUURDUDRUDDRUULURULDRLDRDDDLDLRLUDDRURULRLRLLLDLRRUDLLLLRLULDLUUDUUDRDLRRULLRDRLRLUUDDRRLLDDRULLLRLLURDLRRRRRLLDDRRDLDULDULLDLULLURURRLULRLRLLLLURDDRDDDUUDRRRDUUDDLRDLDRRLLRURUDUUUDLDUULLLRLURULRULRDRLLLDLDLRDRDLLLRUURDDUDDLULRULDLRULUURLLLRRLLLLLLRUURRLULRUUUDLDUDLLRRDDRUUUURRRDRRDULRDUUDULRRRDUUUUURRDUURRRRLDUDDRURULDDURDDRDLLLRDDURUDLLRURLRRRUDDLULULDUULURLUULRDLRDUDDRUULLLRURLDLRRLUDLULDRLUDDDRURUULLDLRLLLDULUDDRLRULURLRDRRDDLDLURUDDUUURRDDLUDDRDUULRRDLDRLLLULLRULRURULRLULULRDUD
      |RUDLLUDRRDRRLRURRULRLRDUDLRRLRDDUDRDLRRLLRURRDDLRLLRRURULRUULDUDUULDULDLRLRDLRDLRUURLDRLUDRRDDDRDRRRDDLLLRRLULLRRDDUDULRDRDUURLDLRULULUDLLDRUDUURRUDLLRDRLRRUUUDLDUDRRULLDURRDUDDLRURDLDRLULDDURRLULLRDDDRLURLULDLRUDLURDURRUDULDUUDLLLDDDUUURRRDLLDURRDLULRULULLRDURULLURDRLLRUUDDRRUDRDRRRURUUDLDDRLDRURULDDLLULULURDLDLDULLRLRDLLUUDDUDUDDDDRURLUDUDDDRRUDDLUDULLRDLDLURDDUURDLRLUUDRRULLRDLDDDLDULDUDRDUUULULDULUDLULRLRUULLDURLDULDRDLLDULLLULRLRD""".stripMargin
