package aoc
package y2018

object Day11:
  import scala.collection.parallel.CollectionConverters.*

  extension (cell: Coord)
    def power(grid: Int): Long =
      def hundreds(i: Long): Long = i % 1000 / 100

      val rackId = cell.x + 10
      hundreds(((rackId * cell.y) + grid) * rackId) - 5

  def solve(grid: Int, size: Long = 3): (Long, Coord) =
    (for
      x <- 1L to (301 - size)
      y <- 1L to (301 - size)
      power = (for
        dx <- 0L until size
        dy <- 0L until size
      yield Coord(x + dx, y + dy)).foldLeft(0L) { (acc, cell) =>
        acc + cell.power(grid)
      }
    yield power -> Coord(x, y))
      .maxBy(_._1)

  def solve2(grid: Int): (Coord, Long) =
    val max = (for
      size <- (1L to 300L).toSeq.par
      (power, cell) = solve(grid, size)
    yield (power, cell, size))
      .maxBy(_._1)
    max._2 -> max._3
