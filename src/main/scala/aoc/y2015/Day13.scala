package aoc
package y2015

object Day13:
  private val linePattern =
    "^([A-Z][a-z]+) would (gain|lose) (\\d+) happiness units by sitting next to ([A-Z][a-z]+)\\.$".r

  def solve(input: String): Int =
    val rels = (for
      case linePattern(first, gl, points, second) <- input.linesIterator
      adjPoints = if gl == "gain" then points.toInt else -points.toInt
    yield (first -> second, adjPoints)).toMap
    rels.keySet
      .map(_._1)
      .toList
      .permutations
      .map(_.pairs.map((a, b) => rels((a, b)) + rels((b, a))).sum)
      .max

  def solve2(input: String): Int =
    val rels = (for
      case linePattern(first, gl, points, second) <- input.linesIterator
      adjPoints = if gl == "gain" then points.toInt else -points.toInt
    yield (first -> second, adjPoints)).toMap
    val guests     = rels.keySet.map(_._1)
    val relsWithMe = guests.foldLeft(rels): (acc, guest) =>
      acc ++ Map(("Me" -> guest) -> 0, (guest -> "Me") -> 0)
    ("Me" :: guests.toList).permutations
      .map(_.pairs.map((a, b) => relsWithMe((a, b)) + relsWithMe((b, a))).sum)
      .max

  val input = """Alice would lose 57 happiness units by sitting next to Bob.
                |Alice would lose 62 happiness units by sitting next to Carol.
                |Alice would lose 75 happiness units by sitting next to David.
                |Alice would gain 71 happiness units by sitting next to Eric.
                |Alice would lose 22 happiness units by sitting next to Frank.
                |Alice would lose 23 happiness units by sitting next to George.
                |Alice would lose 76 happiness units by sitting next to Mallory.
                |Bob would lose 14 happiness units by sitting next to Alice.
                |Bob would gain 48 happiness units by sitting next to Carol.
                |Bob would gain 89 happiness units by sitting next to David.
                |Bob would gain 86 happiness units by sitting next to Eric.
                |Bob would lose 2 happiness units by sitting next to Frank.
                |Bob would gain 27 happiness units by sitting next to George.
                |Bob would gain 19 happiness units by sitting next to Mallory.
                |Carol would gain 37 happiness units by sitting next to Alice.
                |Carol would gain 45 happiness units by sitting next to Bob.
                |Carol would gain 24 happiness units by sitting next to David.
                |Carol would gain 5 happiness units by sitting next to Eric.
                |Carol would lose 68 happiness units by sitting next to Frank.
                |Carol would lose 25 happiness units by sitting next to George.
                |Carol would gain 30 happiness units by sitting next to Mallory.
                |David would lose 51 happiness units by sitting next to Alice.
                |David would gain 34 happiness units by sitting next to Bob.
                |David would gain 99 happiness units by sitting next to Carol.
                |David would gain 91 happiness units by sitting next to Eric.
                |David would lose 38 happiness units by sitting next to Frank.
                |David would gain 60 happiness units by sitting next to George.
                |David would lose 63 happiness units by sitting next to Mallory.
                |Eric would gain 23 happiness units by sitting next to Alice.
                |Eric would lose 69 happiness units by sitting next to Bob.
                |Eric would lose 33 happiness units by sitting next to Carol.
                |Eric would lose 47 happiness units by sitting next to David.
                |Eric would gain 75 happiness units by sitting next to Frank.
                |Eric would gain 82 happiness units by sitting next to George.
                |Eric would gain 13 happiness units by sitting next to Mallory.
                |Frank would gain 77 happiness units by sitting next to Alice.
                |Frank would gain 27 happiness units by sitting next to Bob.
                |Frank would lose 87 happiness units by sitting next to Carol.
                |Frank would gain 74 happiness units by sitting next to David.
                |Frank would lose 41 happiness units by sitting next to Eric.
                |Frank would lose 99 happiness units by sitting next to George.
                |Frank would gain 26 happiness units by sitting next to Mallory.
                |George would lose 63 happiness units by sitting next to Alice.
                |George would lose 51 happiness units by sitting next to Bob.
                |George would lose 60 happiness units by sitting next to Carol.
                |George would gain 30 happiness units by sitting next to David.
                |George would lose 100 happiness units by sitting next to Eric.
                |George would lose 63 happiness units by sitting next to Frank.
                |George would gain 57 happiness units by sitting next to Mallory.
                |Mallory would lose 71 happiness units by sitting next to Alice.
                |Mallory would lose 28 happiness units by sitting next to Bob.
                |Mallory would lose 10 happiness units by sitting next to Carol.
                |Mallory would gain 44 happiness units by sitting next to David.
                |Mallory would gain 22 happiness units by sitting next to Eric.
                |Mallory would gain 79 happiness units by sitting next to Frank.
                |Mallory would lose 16 happiness units by sitting next to George.""".stripMargin
