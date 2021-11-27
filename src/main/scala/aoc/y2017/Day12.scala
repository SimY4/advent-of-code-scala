package aoc
package y2017

import scala.annotation.tailrec

object Day12:
  private type Group = Set[Int]

  def group(program: Int, input: Map[Int, Seq[Int]]): Group =
    @tailrec def group0(acc: Set[Int], current: Set[Int]): Group =
      if current.isEmpty then
        acc
      else
        val connects = acc ++ current.flatMap(input)
        group0(connects, connects.diff(acc))

    group0(Set(program), Set(program))

  val map = input.linesIterator.map { line =>
    "\\d+".r.findAllIn(line).map(_.toInt).toList match
      case i :: is => i -> is
  }.toMap
  val gr   = group(0, map)
  val diff = map.keySet.diff(gr)
  println(diff.size -> (map.keySet.size - diff.size))

  // PART 2

  def groups(input: Map[Int, Seq[Int]]): Set[Group] =
    @tailrec def groups0(acc: Set[Group], i: Map[Int, Seq[Int]]): Set[Group] =
      if i.isEmpty then acc
      else
        val groups = acc + group(i.keySet.head, i)
        groups0(groups, i -- groups.flatten)

    groups0(Set.empty, input)

  val grs = groups(map)
  println(grs.size)

  val input = """0 <-> 2
                |1 <-> 1
                |2 <-> 0, 3, 4
                |3 <-> 2, 4
                |4 <-> 2, 3, 6
                |5 <-> 6
                |6 <-> 4, 5""".stripMargin
