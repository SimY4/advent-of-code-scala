package aoc.y2020

object Day15:
  def solve(input: String, n: Int = 2020): Int =
    val init = input.split(',').map(_.toInt)
    LazyList
      .from(0)
      .scanLeft(Map.empty[Int, List[Int]] -> 0):
        case ((acc, last), i) if i < init.length => acc.updated(init(i), List(i + 1)) -> init(i)
        case ((acc, last), i) =>
          acc.get(last).flatMap(_.dropWhile(_ >= i).headOption) match
            case Some(j) =>
              val v = i - j
              acc.updatedWith(v) {
                case Some(js) => Some((i + 1) :: js)
                case None     => Some(List(i + 1))
              } -> v
            case None =>
              acc.updatedWith(0) {
                case Some(js) => Some((i + 1) :: js)
                case None     => Some(List(i + 1))
              } -> 0
      .drop(n)
      .head
      ._2

  def solve2(input: String): Int = solve(input, 30000000)

  val input = "16,11,15,0,1,7"
