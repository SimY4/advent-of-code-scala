package aoc
package y2017

import scala.util.Try

object Day7 {
  import day7.TreeParser

  type Tree = (Map[String, TreeNode], String)
  case class TreeNode(weight: Int, children: Seq[String])

  private val input = """pbga (66)
                        |xhth (57)
                        |ebii (61)
                        |havc (66)
                        |ktlj (57)
                        |fwft (72) -> ktlj, cntj, xhth
                        |qoyq (66)
                        |padx (45) -> pbga, havc, qoyq
                        |tknk (41) -> ugml, padx, fwft
                        |jptl (61)
                        |ugml (68) -> gyxo, ebii, jptl
                        |gyxo (61)
                        |cntj (57)""".stripMargin

  def tree(input: String): Try[Tree] = {
    def findRoot(nodes: Map[String, TreeNode]): String =
      (for {
        (name, _) <- nodes
        children  = nodes.values.flatMap(_.children).toSet
        if !(children contains name)
      } yield name).head

    for {
      nodes <- new TreeParser(input).Input.run()
      root  = findRoot(nodes)
    } yield nodes -> root
  }

  println(tree(input))

  // PART 2

  def findUnbalancedSubtree(tree: Tree): Option[String] = {
    val (nodes, root) = tree

    def calculateWeight(node: String): Int = nodes(node) match {
      case TreeNode(weight, Nil)      => weight
      case TreeNode(weight, children) => weight + children.map(calculateWeight).sum
    }

    def findUnbalancedSubtree0(node: String): Option[String] = {
      val weightsToNodes = nodes(node).children.groupBy(calculateWeight)
      println(s"$node -> ${weightsToNodes.mapValues(_.map(n => n -> nodes(n)))}")

      weightsToNodes.size match {
        case 0 => None
        case 1 => Some(node)
        case 2 =>
          weightsToNodes.values.toList
            .minBy(_.size)
            .flatMap(findUnbalancedSubtree0)
            .headOption
      }
    }

    findUnbalancedSubtree0(root)
  }

  println(for {
    tree       <- tree(input)
    unbalanced = findUnbalancedSubtree(tree)
  } yield unbalanced)

}

package day7 {
  import Day7._
  import org.parboiled2._

  class TreeParser(val input: ParserInput) extends Parser {

    def Input: Rule1[Map[String, TreeNode]] = rule { Lines ~ EOI }

    private def Lines = rule { oneOrMore(Node).separatedBy(NewLine) ~> (_.toMap) }

    private def Node = rule {
      Name ~ ' ' ~ Weight ~ optional(" -> " ~ Children) ~> { (name, weight, children) =>
        name -> TreeNode(weight, children.getOrElse(Nil))
      }
    }

    private def Children = rule { oneOrMore(Name).separatedBy(", ") }

    private def Name = rule { capture(oneOrMore(CharPredicate.Alpha)) }

    private def Weight = rule { '(' ~ capture(oneOrMore(CharPredicate.Digit)) ~ ')' ~> (_.toInt) }

    private def NewLine = rule { optional('\r') ~ '\n' }

  }

}
