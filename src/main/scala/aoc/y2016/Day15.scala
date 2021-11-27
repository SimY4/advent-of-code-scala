package aoc.y2016

object Day15:
  private case class Disc(n: Int, posNum: Int, initPos: Int)

  private def parseLine(line: String): Disc = line match
    case s"Disc #$n has $posNum positions; at time=0, it is at position $initPos." =>
      Disc(n.toInt, posNum.toInt, initPos.toInt)

  def solve(input: String): Option[Int] =
    val disks = input.linesIterator.map(parseLine).toList
    LazyList.from(0).find { time =>
      disks.forall { disk =>
        (disk.initPos + time + disk.n) % disk.posNum == 0
      }
    }

  def solve2(input: String): Option[Int] =
    val disks = input.linesIterator.map(parseLine).toList :+ Disc(7, 11, 0)
    LazyList.from(0).find { time =>
      disks.forall { disk =>
        (disk.initPos + time + disk.n) % disk.posNum == 0
      }
    }

  val input = """Disc #1 has 13 positions; at time=0, it is at position 11.
                |Disc #2 has 5 positions; at time=0, it is at position 0.
                |Disc #3 has 17 positions; at time=0, it is at position 11.
                |Disc #4 has 3 positions; at time=0, it is at position 0.
                |Disc #5 has 7 positions; at time=0, it is at position 2.
                |Disc #6 has 19 positions; at time=0, it is at position 17.""".stripMargin
