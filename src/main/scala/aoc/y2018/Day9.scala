package aoc.y2018

import scala.annotation.tailrec

object Day9:
  final private class Node(val value: Long):
    self =>
    var prev: Node = self
    var next: Node = self

    infix def +(value: Int): Node =
      val oldNext = next
      val newNode = new Node(value)
      newNode.prev = self
      newNode.next = oldNext
      next = newNode
      oldNext.prev = newNode
      newNode

    def remove: Node =
      val oldNext = next
      val oldPrev = prev
      oldPrev.next = oldNext
      oldNext.prev = oldPrev
      oldNext

  def solve(players: Int, lastMarble: Int = 72170): Long =
    @tailrec def loop(marble: Int = 1, ring: Node = new Node(0), scores: Map[Int, Long] = Map.empty): Long =
      if marble > lastMarble then scores.values.max
      else if 0 == marble % 23 then
        val newRing = (1 to 7).foldLeft(ring)((r, _) => r.prev)
        val removed = newRing.value
        val player  = marble % players
        loop(marble + 1, newRing.remove, scores.updated(player, scores.getOrElse(player, 0L) + marble + removed))
      else loop(marble + 1, ring.next + marble, scores)

    loop()

  def solve2(players: Int): Long = solve(players, 72170 * 100)

  val input = 470
