package aoc
package y2016

object Day13 {
  private def isWall(coord: Coord): Boolean = {
    val f = (coord.x * coord.x) + (3L * coord.x) + (2L * coord.x * coord.y) + coord.y + (coord.y * coord.y) + input
    (f.toBinaryString.count('1' == _) & 1) != 0
  }

  def solve: Option[Int] = {
    def loop(current: Coord, visited: Set[Coord] = Set.empty): Option[Int] =
      if destination == current then Some(0)
      else {
        val newVisited = visited + current
        val neighbours = current
          .neighbours(Direction.hvOnly)
          .filter(n => n.x >= 0 && n.y >= 0 && !newVisited.contains(n))
          .filterNot(isWall)

        neighbours
          .flatMap(loop(_, newVisited))
          .minOption
          .map(1 + _)
      }

    loop(Coord(1L, 1L))
  }

  def solve2: Int = {
    def loop(current: Coord, step: Int = 0, visited: Set[Coord] = Set.empty): Set[Coord] =
      if step > 50 then visited
      else {
        val newVisited = visited + current
        val neighbours = current
          .neighbours(Direction.hvOnly)
          .filter(n => n.x >= 0 && n.y >= 0 && !newVisited.contains(n))
          .filterNot(isWall)

        neighbours
          .map(loop(_, step + 1, newVisited))
          .foldLeft(newVisited)(_ union _)
      }

    loop(Coord(1L, 1L)).size
  }

  val destination = Coord(31L, 39L)
  val input       = 1362L
}
