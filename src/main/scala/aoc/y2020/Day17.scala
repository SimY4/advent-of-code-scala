package aoc.y2020

object Day17:
  import scala.collection.parallel.CollectionConverters.*

  final private case class Coord(x: Long, y: Long, z: Long, w: Long)
  extension (c: Coord)
    private def neighbours: List[Coord] =
      (for
        x <- -1 to 1
        y <- -1 to 1
        z <- -1 to 1
        w <- -1 to 1
        if x != 0 || y != 0 || z != 0 || w != 0
      yield c + Coord(x, y, z, w)).toList

    private def +(other: Coord): Coord = Coord(c.x + other.x, c.y + other.y, c.z + other.z, c.w + other.w)

  extension [A](v: Vector[Vector[Vector[A]]])
    private def get3(c: Coord): Option[A] =
      for
        ys <- v.lift(c.z.toInt)
        xs <- ys.lift(c.y.toInt)
        x  <- xs.lift(c.x.toInt)
      yield x

  extension [A](v: Vector[Vector[Vector[Vector[A]]]])
    private def get4(c: Coord): Option[A] =
      for
        zs <- v.lift(c.w.toInt)
        ys <- zs.lift(c.z.toInt)
        xs <- ys.lift(c.y.toInt)
        x  <- xs.lift(c.x.toInt)
      yield x

  def solve(input: String): Int =
    val initial = Vector.fill(10)(Vector.empty) ++ Vector(
      Vector
        .fill(20)(Vector.empty) ++ input.linesIterator.map(Vector.fill(20)(false) ++ _.map(_ == '#').toVector).toVector
    )

    LazyList
      .from(0)
      .scanLeft(initial) { (state, i) =>
        (0L until 20L).map { z =>
          (0L until 40L).map { y =>
            (0L until 40L).map { x =>
              val coord = Coord(x, y, z, 0L)
              val neighbours = coord.neighbours
                .filter(n => (0 != n.x || 0 != n.y || 0 != n.z) && 0 == n.w)
                .count(n => state.get3(n).exists(identity))

              if state.get3(coord).exists(identity) then neighbours == 2 || neighbours == 3
              else neighbours == 3
            }.toVector
          }.toVector
        }.toVector
      }
      .drop(6)
      .head
      .map(_.map(_.count(identity)).sum)
      .sum

  def solve2(input: String): Int =
    val initial = Vector.fill(10)(Vector.empty) ++ Vector(
      Vector.fill(10)(Vector.empty) ++ Vector(
        Vector.fill(20)(Vector.empty) ++ input.linesIterator
          .map(Vector.fill(20)(false) ++ _.map(_ == '#').toVector)
          .toVector
      )
    )

    LazyList
      .from(0)
      .scanLeft(initial) { (state, i) =>
        (0L until 20L).par.map { w =>
          (0L until 20L).par.map { z =>
            (0L until 40L).par.map { y =>
              (0L until 40L).map { x =>
                val coord = Coord(x, y, z, w)
                val neighbours = coord.neighbours
                  .count(n => state.get4(n).exists(identity))
                if state.get4(coord).exists(identity) then neighbours == 2 || neighbours == 3
                else neighbours == 3
              }.toVector
            }.toVector
          }.toVector
        }.toVector
      }
      .drop(6)
      .head
      .map(_.map(_.map(_.count(identity)).sum).sum)
      .sum

  val input = """...#..#.
                |#..#...#
                |.....###
                |##....##
                |......##
                |........
                |.#......
                |##...#..""".stripMargin
