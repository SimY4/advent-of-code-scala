package aoc.y2018

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day9 {
  private final class Node(val value: Long) { self =>
    var prev: Node                = self
    var next: Node                = self
    override def toString: String = value.toString
  }

  private object Node {
    def apply(prev: Node, value: Int, next: Node): Node = {
      val newNode = new Node(value)
      newNode.prev = prev
      newNode.next = next
      newNode
    }
      
    def (node: Node) + (value: Int): Node = {
      val oldNext = node.next
      val newNode = Node(node, value, oldNext)
      node.next = newNode
      oldNext.prev = newNode
      newNode
    }

    def (node: Node) remove: Node = {
      val oldNext = node.next
      val oldPrev = node.prev
      oldPrev.next = oldNext
      oldNext.prev = oldPrev
      oldNext
    }
  }
  
  import Node._

  def solve(players: Int, marbles: Int): Long = {
    @tailrec def solve0(marble: Int, ring: Node, scores: Map[Int, Long]): Long =
      if (marble > marbles) scores.values.max
      else if (0 == marble % 23) {
        val newRing = (1 to 7).foldLeft(ring) { (r, _) =>
          r.prev
        }
        val removed = newRing.value
        val player  = (marble % players)
        solve0(marble + 1, newRing.remove, scores + (player -> (scores.getOrElse(player, 0L) + marble + removed)))
      } 
      else solve0(marble + 1, ring.next + marble, scores)

    solve0(1, new Node(0), Map.empty)
  }
}