package aoc.y2021

import scala.annotation.tailrec

object Day12:
  def parseInput(input: String): Map[String, Vector[String]] = input.linesIterator
    .flatMap:
      case s"start-$to"   => List("start" -> to)
      case s"$from-start" => List("start" -> from)
      case s"end-$to"     => List(to -> "end")
      case s"$from-end"   => List(from -> "end")
      case s"$from-$to"   => List(from -> to, to -> from)
    .toVector
    .groupMap(_(0))(_(1))

  def solve(input: String): Int =
    val paths = parseInput(input)

    @tailrec def loop(visited: List[List[String]]): Int =
      val newPaths = visited.flatMap:
        case Nil               => Nil
        case path @ "end" :: _ => List(path)
        case path              => paths(path.head).map(_ :: path)
      if newPaths == visited then visited.size
      else
        loop(
          newPaths.filter(
            _.filter(_.forall(_.isLower)).groupMapReduce(identity)(_ => 1)(_ + _).values.forall(_ < 2)
          )
        )

    loop(List(List("start")))

  def solve2(input: String): Int =
    val paths = parseInput(input)

    @tailrec def loop(visited: List[List[String]]): Int =
      val newPaths = visited.flatMap:
        case Nil               => Nil
        case path @ "end" :: _ => List(path)
        case path              => paths(path.head).map(_ :: path)
      if newPaths == visited then visited.size
      else
        loop(
          newPaths.filter: path =>
            val small = path.filter(_.forall(_.isLower)).groupMapReduce(identity)(_ => 1)(_ + _).values.toVector
            small.count(_ == 2) < 2 && small.filter(_ != 2).forall(_ < 2)
        )

    loop(List(List("start")))

  val input = """pq-GX
                |GX-ah
                |mj-PI
                |ey-start
                |end-PI
                |YV-mj
                |ah-iw
                |te-GX
                |te-mj
                |ZM-iw
                |te-PI
                |ah-ZM
                |ey-te
                |ZM-end
                |end-mj
                |te-iw
                |te-vc
                |PI-pq
                |PI-start
                |pq-ey
                |PI-iw
                |ah-ey
                |pq-iw
                |pq-start
                |mj-GX""".stripMargin
