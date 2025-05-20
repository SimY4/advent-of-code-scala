package aoc
package y2017

import scala.annotation.tailrec

object Day12 extends Input(2017, 12):
  private def group(program: Int, input: Map[Int, Seq[Int]]): Set[Int] =
    @tailrec def loop(acc: Set[Int], current: Set[Int]): Set[Int] =
      if current.isEmpty then acc
      else
        val connects = acc ++ current.flatMap(input)
        loop(connects, connects.diff(acc))

    loop(Set.empty, Set(program))

  def solve(input: String): Int =
    val map = input.linesIterator
      .map: line =>
        "\\d+".r.findAllIn(line).map(_.toInt).toList match
          case i :: is => i -> is
      .toMap

    val gr   = group(0, map)
    val diff = map.keySet.diff(gr)
    map.keySet.size - diff.size

  private def groups(input: Map[Int, Seq[Int]]): Set[Set[Int]] =
    @tailrec def loop(acc: Set[Set[Int]], input: Map[Int, Seq[Int]]): Set[Set[Int]] =
      if input.isEmpty then acc
      else
        val groups = acc + group(input.keySet.head, input)
        loop(groups, input -- groups.flatten)

    loop(Set.empty, input)

  def solve2(input: String): Int =
    val map = input.linesIterator
      .map: line =>
        "\\d+".r.findAllIn(line).map(_.toInt).toList match
          case i :: is => i -> is
      .toMap
    val grs = groups(map)
    grs.size
