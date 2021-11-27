package aoc
package y2015

object Day20:
  def solve(input: Int): Int =
    LazyList
      .iterate(0L)(_ + 1)
      .map(_.factors.sum * 10)
      .indexWhere(input <= _)

  def solve2(input: Int): Int =
    LazyList
      .iterate(0L)(_ + 1)
      .map(house => house.factors.filter(house / _ <= 50).sum * 11)
      .indexWhere(input <= _)

  val input = 29000000
