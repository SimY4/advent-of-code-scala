package aoc
package y2022

object Day15:
  private def range(y: Long)(sensor: Coord, closestBeacon: Coord): Option[(Long, Long)] =
    val dist   = sensor.manhattan(closestBeacon)
    val offset = dist - math.abs(sensor.y - y)
    if offset < 0L then None
    else
      val minX = sensor.x - offset
      val maxX = sensor.x + offset
      Some(minX -> maxX)

  def solve(input: String, y: Long = 2000000L): Int =
    val map = input.linesIterator.map { line =>
      "-?\\d+".r.findAllIn(line).toList match
        case sx :: sy :: cbx :: cby :: Nil =>
          Coord(sx.toLong, sy.toLong) -> Coord(cbx.toLong, cby.toLong)
    }.toVector
    val beacons = map.collect { case (_, Coord(x, `y`)) => x }.distinct.size

    map.view.flatMap(range(y)).flatMap(_ to _).toSet.size - beacons

  def solve2(input: String, searchArea: Long = 4000000L): Long =
    val map = input.linesIterator.map { line =>
      "-?\\d+".r.findAllIn(line).toList match
        case sx :: sy :: cbx :: cby :: Nil =>
          Coord(sx.toLong, sy.toLong) -> Coord(cbx.toLong, cby.toLong)
    }.toVector

    (for
      y <- LazyList.range(0L, searchArea)
      ranges = map
        .flatMap(range(y))
        .sorted
        .foldLeft(Nil: List[(Long, Long)]) {
          case (Nil, interval) => interval :: Nil
          case ((topStart, topEnd) :: tail, (nextStart, nextEnd)) if nextStart <= topEnd || topEnd + 1 == nextStart =>
            if nextEnd > topEnd then (topStart, nextEnd) :: tail
            else (topStart, topEnd) :: tail
          case (acc, (start, end)) if end < 0 || searchArea < start => acc
          case (acc, interval)                                      => interval :: acc
        }
        .reverse
      case (_, x) :: _ <- Some(ranges).filter(_.size > 1)
    yield ((x + 1) * 4000000L) + y).head

  val input = """Sensor at x=2662540, y=1992627: closest beacon is at x=1562171, y=2000000
                |Sensor at x=3577947, y=3994226: closest beacon is at x=3468220, y=3832344
                |Sensor at x=34015, y=3658022: closest beacon is at x=-48386, y=3887238
                |Sensor at x=3951270, y=2868430: closest beacon is at x=3499312, y=2620002
                |Sensor at x=3136779, y=3094333: closest beacon is at x=2731027, y=3076619
                |Sensor at x=3415109, y=2591103: closest beacon is at x=3499312, y=2620002
                |Sensor at x=277465, y=3971183: closest beacon is at x=-48386, y=3887238
                |Sensor at x=3697201, y=1834735: closest beacon is at x=3499312, y=2620002
                |Sensor at x=874397, y=1535447: closest beacon is at x=1562171, y=2000000
                |Sensor at x=2996230, y=3508199: closest beacon is at x=3251079, y=3709457
                |Sensor at x=2754388, y=3147571: closest beacon is at x=2731027, y=3076619
                |Sensor at x=524580, y=2640616: closest beacon is at x=-73189, y=1870650
                |Sensor at x=2718599, y=3106610: closest beacon is at x=2731027, y=3076619
                |Sensor at x=2708759, y=3688992: closest beacon is at x=3251079, y=3709457
                |Sensor at x=2413450, y=3994713: closest beacon is at x=3251079, y=3709457
                |Sensor at x=1881113, y=495129: closest beacon is at x=1562171, y=2000000
                |Sensor at x=3792459, y=3827590: closest beacon is at x=3468220, y=3832344
                |Sensor at x=3658528, y=641189: closest beacon is at x=4097969, y=-110334
                |Sensor at x=1379548, y=3381581: closest beacon is at x=1562171, y=2000000
                |Sensor at x=3480959, y=3069234: closest beacon is at x=3499312, y=2620002
                |Sensor at x=3871880, y=3531918: closest beacon is at x=3468220, y=3832344
                |Sensor at x=2825206, y=2606984: closest beacon is at x=2731027, y=3076619
                |Sensor at x=3645217, y=2312011: closest beacon is at x=3499312, y=2620002
                |Sensor at x=3485320, y=3509352: closest beacon is at x=3468220, y=3832344
                |Sensor at x=56145, y=3879324: closest beacon is at x=-48386, y=3887238
                |Sensor at x=148776, y=433043: closest beacon is at x=-73189, y=1870650
                |Sensor at x=3368682, y=3929248: closest beacon is at x=3468220, y=3832344
                |Sensor at x=3330787, y=2481990: closest beacon is at x=3499312, y=2620002
                |Sensor at x=2802875, y=3209067: closest beacon is at x=2731027, y=3076619
                |Sensor at x=2679788, y=3102108: closest beacon is at x=2731027, y=3076619
                |Sensor at x=3326846, y=3767097: closest beacon is at x=3251079, y=3709457
                |Sensor at x=3111518, y=1310720: closest beacon is at x=3499312, y=2620002""".stripMargin
