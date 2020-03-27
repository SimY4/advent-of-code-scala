package object aoc {
  def [A] (as: List[A]) pairs: List[(A, A)] = 
    (as.head -> as.last) :: (as zip as.tail)

  final case class Coord(x: Long, y: Long)
  object Coord {
    extension ops on (coord: Coord) {
      def dist(to: Coord): Long =
        math.abs(to.x - coord.x) + math.abs(to.y - coord.y)

      def distance: Long = math.max(math.max(math.abs(coord.x), math.abs(coord.y)), math.abs(coord.x - coord.y))
    }
  }
}