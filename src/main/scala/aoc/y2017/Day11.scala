package aoc
package y2017

import scala.language.implicitConversions

object Day11:
  private enum Direction:
    case N, NE, SE, S, SW, NW

  import Direction.*

  private def parse(input: String): Seq[Direction] = input.split(',').toSeq.map {
    case "n"  => N
    case "ne" => NE
    case "se" => SE
    case "s"  => S
    case "sw" => SW
    case "nw" => NW
  }

  private def track(current: Coord, direction: Direction): Coord = direction match
    case N  => current.copy(x = current.x + 1)
    case NE => current.copy(current.x + 1, current.y + 1)
    case SE => current.copy(y = current.y + 1)
    case S  => current.copy(x = current.x - 1)
    case SW => current.copy(current.x - 1, current.y - 1)
    case NW => current.copy(y = current.y - 1)

  private def optimise(path: Seq[Direction], max: Coord = Coord(0, 0)): (Long, Long) =
    val start = Coord(0, 0)
    val (end, max) = path.foldLeft((start, 0L)) { case ((current, mx), direction) =>
      val next         = track(current, direction)
      val nextDistance = next.distance
      if nextDistance > mx then (next, nextDistance)
      else (next, mx)
    }
    println(end)
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

