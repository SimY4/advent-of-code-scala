package object aoc {
  extension [A](as: List[A]) def pairs: List[(A, A)] = 
    (as.head -> as.last) :: (as zip as.tail)

  extension (n: Long) def factors: Seq[Long] = 
    (1L to math.sqrt(n.toDouble).toLong)
      .flatMap { i => 
        if (n % i == 0L) {
          val div = n / i
          if (i != div) i :: div :: Nil
          else i :: Nil
        } else Nil
      }
      
  private val hexArray = "0123456789ABCDEF".toCharArray

  extension (bytes: Array[Byte]) def printHexBinary: String = {
    val hexChars = new Array[Char](bytes.length * 2)
    for (i <- 0 until bytes.length) {
      val v = bytes(i) & 0xFF
      hexChars(i * 2) = hexArray(v >>> 4)
      hexChars(i * 2 + 1) = hexArray(v & 0x0F)
    }
    new String(hexChars)
  }

  final case class Coord(x: Long, y: Long)
  object Coord {
    extension (coord: Coord) {
      def + (other: Coord): Coord =
        Coord(coord.x + other.x, coord.y + other.y)

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