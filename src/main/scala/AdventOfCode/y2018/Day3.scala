package AdventOfCode
package y2018

object Day3 {

  val input = """#1 @ 1,3: 4x4
                |#2 @ 3,1: 4x4
                |#3 @ 5,5: 2x2""".stripMargin

  case class Claim(id: Int, h1: Int, v1: Int, h2: Int, v2: Int)

  def fabric(input: String): Array[Array[Set[Int]]] = {
    def parse(line: String): Claim =
      "\\d+".r.findAllIn(line).map(_.toInt).toList match {
        case id :: h1 :: v1 :: h2 :: v2 :: Nil => Claim(id, h1, v1, h2, v2)
      }
    val fabric: Array[Array[Set[Int]]] = Array.fill(1000, 1000)(Set.empty[Int])
    input.linesIterator.map(parse).foreach { claim =>
      (for {
        i <- claim.v1 until claim.v1 + claim.v2
        j <- claim.h1 until claim.h1 + claim.h2
      } yield (i, j)).foreach {
        case (i, j) =>
          fabric(i)(j) = fabric(i)(j) + claim.id
      }
    }
    fabric
  }

  def solve(input: String): Int = fabric(input).foldLeft(0) { (i, arr) =>
    i + arr.count(_.size > 1)
  }

  def solve2(input: String): Int = {
    val res = Array.fill(input.linesIterator.size)(true)
    fabric(input).foreach { arr =>
      arr.foreach { ids =>
        if (ids.size > 1) {
          ids.foreach { id =>
            res(id - 1) = false
          }
        }
      }
    }
    res.indexOf(true) + 1
  }

}
