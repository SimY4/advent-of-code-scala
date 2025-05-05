package aoc.y2021

object Day14:
  def solve(input: String): Int =
    val template = input.linesIterator.next.toVector
    val pairs = input.linesIterator
      .drop(2)
      .map:
        case s"$x -> $y" => (x.head, x.last) -> y.head
      .toMap

    val polymer = LazyList
      .iterate(template): template =>
        template
          .zip(template.tail)
          .flatMap: pair =>
            pairs.get(pair) match
              case Some(subs) => List(pair(0), subs)
              case None       => List(pair(0))
          .appended(template.last)
      .drop(10)
      .head

    val counts = polymer.groupMapReduce(identity)(_ => 1)(_ + _)
    counts.values.max - counts.values.min

  def solve2(input: String): Long =
    val template = input.linesIterator.next.toVector
    val pairs = input.linesIterator
      .drop(2)
      .map:
        case s"$x -> $y" => (x.head, x.last) -> y.head
      .toMap

    type Cache = Map[(Int, Char, Char), Map[Char, Long]]
    extension (m: Map[Char, Long])
      private def merge(other: Map[Char, Long]): Map[Char, Long] =
        other.keySet.foldLeft(m): (acc, key) =>
          acc.updatedWith(key):
            case None    => Some(other(key))
            case Some(v) => Some(v + other(key))

    def polymerization(depth: Int, pair: (Char, Char), cache: Cache = Map.empty): (Map[Char, Long], Cache) =
      if depth == 0 then (Map(pair(0) -> 1L).merge(Map(pair(1) -> 1L)), cache)
      else
        cache.get(depth *: pair) match
          case None =>
            val (left, lcache)  = polymerization(depth - 1, pair(0) -> pairs(pair), cache)
            val (right, rcache) = polymerization(depth - 1, pairs(pair) -> pair(1), lcache)
            val merged          = left.merge(right).merge(Map(pairs(pair) -> -1L))
            merged -> rcache.updated(depth *: pair, merged)
          case Some(s) => s -> cache

    val counts = template
      .zip(template.tail)
      .map(pair => polymerization(40, pair)(0))
      .reduce(_.merge(_))
      .merge(template.init.tail.map(_ -> -1L).groupMapReduce(_(0))(_(1))(_ + _))

    counts.values.max - counts.values.min

  val input = """CKFFSCFSCBCKBPBCSPKP
                |
                |NS -> P
                |KV -> B
                |FV -> S
                |BB -> V
                |CF -> O
                |CK -> N
                |BC -> B
                |PV -> N
                |KO -> C
                |CO -> O
                |HP -> P
                |HO -> P
                |OV -> O
                |VO -> C
                |SP -> P
                |BV -> H
                |CB -> F
                |SF -> H
                |ON -> O
                |KK -> V
                |HC -> N
                |FH -> P
                |OO -> P
                |VC -> F
                |VP -> N
                |FO -> F
                |CP -> C
                |SV -> S
                |PF -> O
                |OF -> H
                |BN -> V
                |SC -> V
                |SB -> O
                |NC -> P
                |CN -> K
                |BP -> O
                |PC -> H
                |PS -> C
                |NB -> K
                |VB -> P
                |HS -> V
                |BO -> K
                |NV -> B
                |PK -> K
                |SN -> H
                |OB -> C
                |BK -> S
                |KH -> P
                |BS -> S
                |HV -> O
                |FN -> F
                |FS -> N
                |FP -> F
                |PO -> B
                |NP -> O
                |FF -> H
                |PN -> K
                |HF -> H
                |VK -> K
                |NF -> K
                |PP -> H
                |PH -> B
                |SK -> P
                |HN -> B
                |VS -> V
                |VN -> N
                |KB -> O
                |KC -> O
                |KP -> C
                |OS -> O
                |SO -> O
                |VH -> C
                |OK -> B
                |HH -> B
                |OC -> P
                |CV -> N
                |SH -> O
                |HK -> N
                |NO -> F
                |VF -> S
                |NN -> O
                |FK -> V
                |HB -> O
                |SS -> O
                |FB -> B
                |KS -> O
                |CC -> S
                |KF -> V
                |VV -> S
                |OP -> H
                |KN -> F
                |CS -> H
                |CH -> P
                |BF -> F
                |NH -> O
                |NK -> C
                |OH -> C
                |BH -> O
                |FC -> V
                |PB -> B""".stripMargin
