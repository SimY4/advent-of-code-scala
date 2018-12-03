package AdventOfCode
package y2018

object Day3 {

  val input = """#1 @ 1,3: 4x4
                |#2 @ 3,1: 4x4
                |#3 @ 5,5: 2x2""".stripMargin

  case class Claim(h1: Int, v1: Int, h2: Int, v2: Int)

  def parse(line: String): (Int, Claim) = {
    "\\d+".r.findAllIn(line).toList.map(_.toInt) match {
      case id :: h1 :: v1 :: h2 :: v2 :: Nil => id -> Claim(h1, v1, h2, v2)
    }
  }

  val init: Array[Array[Set[Int]]] = Array.fill(1000, 1000)(Set.empty[Int])
  def solve(input: String): Int = {
    val fabric = input.linesIterator.map(parse).foldLeft(init) { (fabric, line) =>
      val (id, claim) = line
      (for {
        i <- claim.v1 until claim.v1 + claim.v2
        j <- claim.h1 until claim.h1 + claim.h2
      } yield (i, j)).foreach { case (i, j) =>
        fabric(i)(j) = fabric(i)(j) + id
      }
      fabric
    }
    fabric.foldLeft(0) { (i, arr) => 
      i + arr.count(_.size > 1) 
    }
  }
  
  def solve2(input: String): Int = {
    val fabric = input.linesIterator.map(parse).foldLeft(init) { (fabric, line) =>
      val (id, claim) = line
      (for {
        i <- claim.v1 until claim.v1 + claim.v2
        j <- claim.h1 until claim.h1 + claim.h2
      } yield (i, j)).foreach { case (i, j) =>
        fabric(i)(j) = fabric(i)(j) + id
      }
      fabric
    }
    val res = Array.fill(input.linesIterator.size)(true)
    fabric.foreach { arr => 
      arr.foreach { ids => 
        if (ids.size > 1) {
          ids.foreach { id => res(id - 1) = false } 
        }
      } 
    }
    res.indexOf(true) + 1
  }

}