package aoc
package y2018

object Day11 {

  case class Cell(x: Int, y: Int)
  implicit class CellOps(private val cell: Cell) extends AnyVal {
    def power(grid: Int): Int = {
      val rackId = cell.x + 10
      hundreds(((rackId * cell.y) + grid) * rackId) - 5
    }

    private[this] def hundreds(i: Int): Int = i % 1000 / 100
  }

  def solve(grid: Int, size: Int = 3): (Int, Cell) =
    (for {
      x <- 1 to (301 - size)
      y <- 1 to (301 - size)
      power = (for {
        dx <- 0 until size
        dy <- 0 until size
      } yield Cell(x + dx, y + dy)).foldLeft(0) { (acc, cell) =>
        acc + cell.power(grid)
      }
    } yield power -> Cell(x, y))
      .maxBy(_._1)

  def solve2(grid: Int): (Cell, Int) = {
    val max = (for {
      size          <- (1 to 300).toSeq.par
      (power, cell) = solve(grid, size)
    } yield (power, cell, size))
      .maxBy(_._1)
    max._2 -> max._3
  }
}
