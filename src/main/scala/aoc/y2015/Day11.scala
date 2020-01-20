package aoc.y2015

object Day11 with
  private val forbidden = Set('i', 'o', 'l')

  private def (s: List[Char]) increment: List[Char] =
    s.reverse match
      case 'z' :: tail => increment(tail.reverse) :+ 'a'
      case x :: tail => ((x + 1).toChar :: tail).reverse
      case Nil => 'a' :: Nil

  private def meetRequirements(pass: List[Char]): Boolean =
    pass
      .sliding(3)
      .exists { triple => 
        triple.size == 3 && triple(0) + 1 == triple(1) && triple(1) + 1 == triple(2) 
      } && 
      !pass.exists(forbidden.contains) && 
      pass
        .sliding(2)
        .zipWithIndex
        .filter(pair => pair.size == 2 && pair._1(0) == pair._1(1))
        .map(_._2)
        .sliding(2)
        .exists(pair => pair.size == 2 && pair(0) + 1 != pair(1))

  def solve(input: String): String =
    LazyList.iterate(input.toList)(_.increment)
      .filter(meetRequirements)
      .head
      .mkString
    
  def solve2(input: String): String = 
    LazyList.iterate(input.toList)(_.increment)
      .filter(meetRequirements)
      .drop(1)
      .head
      .mkString

  val input = "hxbxwxba"