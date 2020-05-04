package object aoc {
  def [A] (as: List[A]) pairs: List[(A, A)] = 
    (as.head -> as.last) :: (as zip as.tail)

  def (n: Long) fartors: Seq[Long] = 
    (1L to math.sqrt(n.toDouble).toLong)
      .flatMap { i => 
        if (n % i == 0L) {
          val div = n / i
          if (i != div) i :: div :: Nil
          else i :: Nil
        } else Nil
      }

  final case class Coord(x: Long, y: Long)
  object Coord {
    extension ops on (coord: Coord) {
      def dist(to: Coord): Long =
        math.abs(to.x - coord.x) + math.abs(to.y - coord.y)

      def distance: Long = math.max(math.max(math.abs(coord.x), math.abs(coord.y)), math.abs(coord.x - coord.y))

      def neighbours: List[Coord] = {
        val Coord(x, y) = coord
        List(
          Coord(x - 1, y - 1),
          Coord(x, y - 1),
          Coord(x + 1, y - 1),
          Coord(x - 1, y),
          Coord(x + 1, y),
          Coord(x - 1, y + 1),
          Coord(x, y + 1),
          Coord(x + 1, y + 1)
        )
      }
    }
  }
}