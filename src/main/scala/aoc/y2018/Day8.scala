package aoc.y2018

object Day8:
  final private case class Node(children: List[Node], meta: List[Int])
  private object Node:
    extension (node: Node)
      def sumMeta: Int =
        node.meta.reduce(_ + _) + node.children.map(_.sumMeta).foldLeft(0)(_ + _)

      def sumMeta2: Int = node.children match
        case Nil => node.meta.reduce(_ + _)
        case ch =>
          node.meta.foldLeft(0) { (acc, m) =>
            acc + node.children.lift(m - 1).fold(0)(_.sumMeta2)
          }

  private def parse(input: String): Node =
    val nums = input.split(" ").map(_.toInt).toList
    def child(list: List[Int]): (Node, List[Int]) = list match
      case num_ch :: num_meta :: rest =>
        val (children, r) = (1 to num_ch).foldLeft(Vector.empty[Node] -> rest) { (childrenTail, _) =>
          val (ch, newRest) = child(childrenTail._2)
          (childrenTail._1 :+ ch) -> newRest
        }
        Node(children.toList, r.take(num_meta)) -> r.drop(num_meta)
    child(nums)._1

  import Node.*

  def solve(input: String): Int = parse(input).sumMeta

  def solve2(input: String): Int = parse(input).sumMeta2

  val input = """2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2""".stripMargin
