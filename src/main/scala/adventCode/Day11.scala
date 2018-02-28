package adventCode

object Day11 {

  sealed trait Direction
  case object N extends Direction
  case object NE extends Direction
  case object SE extends Direction
  case object S extends Direction
  case object SW extends Direction
  case object NW extends Direction

  type Coord = (Int, Int)
  implicit def coordOps(coord: Coord): CoordOps = new CoordOps(coord)
  final class CoordOps(coord: Coord) {
    def distance: Int = math.max(math.max(math.abs(coord._1), math.abs(coord._2)), math.abs(coord._1 - coord._2))
  }

  def parse(input: String): Seq[Direction] = input.split(",").map {
    case "n"  => N
    case "ne" => NE
    case "se" => SE
    case "s"  => S
    case "sw" => SW
    case "nw" => NW
  }

  def track(current: Coord, direction: Direction): Coord = direction match {
    case N  => current._1 + 1 -> current._2
    case NE => (current._1 + 1) -> (current._2 + 1)
    case SE => current._1 -> (current._2 + 1)
    case S  => current._1 - 1 -> current._2
    case SW => (current._1 - 1) -> (current._2 - 1)
    case NW => current._1 -> (current._2 - 1)
  }

  def optimise(path: Seq[Direction], max: Coord = 0 -> 0): (Int, Int) = {
    val start = 0 -> 0
    val (end, max) = path.foldLeft((start, 0)) { case ((current, mx), direction) =>
        val next = track(current, direction)
        val nextDistance = next.distance
        if (nextDistance > mx) {
          (next, nextDistance)
        } else {
          (next, mx)
        }
    }
    println(end)
    (end.distance, max)
  }

  println(for {
    (input, res) <- Map(
      "ne,ne,ne" -> 3,
      "ne,ne,sw,sw" -> 0,
      "ne,ne,s,s" -> 2,
      "se,sw,se,sw,sw" -> 3
    )
    parsed = parse(input)
    r @ (min, _) = optimise(parsed)
    _ = println(r)
    _ = println(min == res)
  } yield r)

}
