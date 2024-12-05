package aoc.y2016

import java.util.BitSet

import scala.annotation.tailrec

object Day19:
  def solve(input: Int): Int =
    val elves = new BitSet(input)
    elves.set(0, input)

    @tailrec def loop(cur: Int): Int =
      var next = elves.nextSetBit(cur + 1)
      next = if next < 0 then elves.nextSetBit(0) else next
      if elves.get(cur) then
        if cur == next then cur
        else
          elves.clear(next)
          loop(next)
      else loop(next)

    loop(0) + 1

  def solve2(input: Int): Int =
    var p = 1
    while 3 * p <= input do p = p * 3
    if input == p then input
    else input - p + math.max(input - 2 * p, 0)

  val input = 3012210
