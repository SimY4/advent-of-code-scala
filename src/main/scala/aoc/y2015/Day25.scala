package aoc
package y2015

object Day25:
  final private case class Cell(coord: Coord, incRow: Long)

  def solve(input: Coord): Option[Long] =
    LazyList
      .iterate(Cell(Coord(1L, 1L), 1L) -> 20151125L) { case (Cell(Coord(row, col), incRow), value) =>
        val nextValue = value * 252533L % 33554393L
        if incRow == col then Cell(Coord(incRow + 1L, 1L), incRow + 1L) -> nextValue
        else Cell(Coord(row - 1L, col + 1L), incRow)                    -> nextValue
      }
      .collectFirst { case (Cell(coord, _), value) if coord == input => value }

  val input = Coord(2947L, 3029L)
