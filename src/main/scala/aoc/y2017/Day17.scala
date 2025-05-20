package aoc.y2017

import scala.collection.mutable

object Day17:
  def solve(input: Int): Int =
    val buffer = mutable.ArrayBuffer(0)
    val finalPos = (1 to 2017).foldLeft(0): (pos, i) =>
      val next = (pos + input) % buffer.size + 1
      buffer.insert(next, i)
      next
    buffer((finalPos + 1) % buffer.size)

  def solve2(input: Int): Int =
    val (_, valueAt) = (1 to 50000000).foldLeft(0 -> 0):
      case ((pos, valueAt), i) =>
        val next = (pos + input) % i + 1
        if next == 1 then next -> i
        else next              -> valueAt
    valueAt

  val input = 370
