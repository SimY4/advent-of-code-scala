package aoc
package y2017

object Day11:
  import Direction.*

  private def parse(input: String): Seq[Direction] = input.split(',').toSeq.map {
    case "n"  => Up
    case "ne" => UpRight
    case "se" => DownRight
    case "s"  => Down
    case "sw" => DownLeft
    case "nw" => UpLeft
  }

  private def track(current: Coord, direction: Direction): Coord = direction match
    case Up        => current.copy(x = current.x + 1)
    case UpRight   => current.copy(current.x + 1, current.y + 1)
    case DownRight => current.copy(y = current.y + 1)
    case Down      => current.copy(x = current.x - 1)
    case DownLeft  => current.copy(current.x - 1, current.y - 1)
    case UpLeft    => current.copy(y = current.y - 1)

  private def optimise(path: Seq[Direction], max: Coord = Coord(0, 0)): (Long, Long) =
    val start = Coord(0, 0)
    val (end, max) = path.foldLeft((start, 0L)) { case ((current, mx), direction) =>
      val next         = track(current, direction)
      val nextDistance = next.distance
      if nextDistance > mx then (next, nextDistance)
      else (next, mx)
    }
    (end.distance, max)

  println(for
    (input, res) <- Map(
      "ne,ne,ne"       -> 3,
      "ne,ne,sw,sw"    -> 0,
      "ne,ne,s,s"      -> 2,
      "se,sw,se,sw,sw" -> 3
    )
    parsed       = parse(input)
    r @ (min, _) = optimise(parsed)
    _            = println(r)
    _            = println(min == res)
  yield r)
