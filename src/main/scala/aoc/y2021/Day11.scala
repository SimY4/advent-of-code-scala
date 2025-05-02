package aoc
package y2021

import scala.annotation.tailrec

object Day11:
  @tailrec private def illuminate(map: Map[Coord, Int], flashed: Set[Coord] = Set.empty): Map[Coord, Int] =
    val newFlashes = (for coord <- map.keySet; if map(coord) > 9 yield coord) &~ flashed
    if newFlashes.isEmpty then map.view.mapValues(il => if il > 9 then 0 else il).toMap
    else
      val newMap = newFlashes.toList
        .flatMap(_.neighbours())
        .foldLeft(map): (acc, neighbour) =>
          acc.updatedWith(neighbour)(_.map(_ + 1))
      illuminate(newMap, flashed | newFlashes)

  def solve(input: String): Int =
    val octopuses = input.linesIterator.zipWithIndex
      .flatMap: (line, y) =>
        line.zipWithIndex.map((ch, x) => Coord(x, y) -> ch.asDigit)
      .toMap

    LazyList
      .unfold(octopuses): octopuses =>
        val next = illuminate(octopuses.view.mapValues(_ + 1).toMap)
        Some(next.count((_, il) => il == 0), next)
      .take(100)
      .sum

  def solve2(input: String): Int =
    val octopuses = input.linesIterator.zipWithIndex
      .flatMap: (line, y) =>
        line.zipWithIndex.map((ch, x) => Coord(x, y) -> ch.asDigit)
      .toMap

    LazyList
      .iterate(octopuses)(octopuses => illuminate(octopuses.view.mapValues(_ + 1).toMap))
      .indexWhere(_.forall((_, il) => il == 0))

  val input = """4134384626
                |7114585257
                |1582536488
                |4865715538
                |5733423513
                |8532144181
                |1288614583
                |2248711141
                |6415871681
                |7881531438""".stripMargin
