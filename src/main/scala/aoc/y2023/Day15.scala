package aoc
package y2023

import scala.collection.immutable

object Day15 extends Input(2023, 15):
  extension (s: String)
    private def hash: Int =
      s.foldLeft(0): (hash, ch) =>
        ((hash + ch.toInt) * 17) % 256

  def solve(input: String): Int =
    input
      .replace("\n", "")
      .split(',')
      .map(_.hash)
      .sum

  def solve2(input: String): Int =
    input
      .replace("\n", "")
      .split(',')
      .foldLeft(Vector.fill(256)(immutable.ListMap.empty[String, Int])):
        case (acc, s"$lens=$len") =>
          val hash = lens.hash
          acc.updated(hash, acc(hash).updated(lens, len.toInt))
        case (acc, s"${lens}-") =>
          val hash = lens.hash
          acc.updated(hash, acc(hash) - lens)
      .zipWithIndex
      .map: (map, i) =>
        (i + 1) * map.values.zipWithIndex.map((len, j) => len * (j + 1)).sum
      .sum
