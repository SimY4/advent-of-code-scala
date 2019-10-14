package aoc.y2015

import scala.annotation.tailrec

object Day10

  private def countGroups(s: List[Char]): List[(Char, Int)] =
    @tailrec def iterate(rest: List[Char], acc: List[(Char, Int)] = Nil): List[(Char, Int)] =
      rest.headOption match
        case Some(head) => 
          iterate(rest.dropWhile(_ == head), (head -> rest.takeWhile(_ == head).size) :: acc)
        case None => acc
    iterate(s).reverse

  def solve(input: String, times: Int = 40): Int =
   (1 to times).foldLeft(input.toList) { (list, _) =>
     countGroups(list)
       .flatMap { case (ch, cnt) => s"${cnt}${ch}".toList }
   }.size

  def solve2(input: String): Int = solve(input, 50)
