package aoc

extension [A](as: List[A])
  def pairs: List[(A, A)] =
    (as.head -> as.last) :: (as.zip(as.tail))

extension (n: Long)
  def factorial: Long = (1L to n).product

  def factors: Seq[Long] =
    (1L to math.sqrt(n.toDouble).toLong).flatMap { i =>
      if n % i == 0L then
        val div = n / i
        if i != div then i :: div :: Nil
        else i :: Nil
      else Nil
    }

private val hexArray = "0123456789ABCDEF".toCharArray

extension (bytes: Array[Byte])
  def printHexBinary: String =
    val hexChars = new Array[Char](bytes.length * 2)
    for i <- 0 until bytes.length do
      val v = bytes(i) & 0xff
      hexChars(i * 2) = hexArray(v >>> 4)
      hexChars(i * 2 + 1) = hexArray(v & 0x0f)
    new String(hexChars)

final case class Coord(x: Long, y: Long)
object Coord:
  extension (coord: Coord)
    def +(other: Coord): Coord =
      Coord(coord.x + other.x, coord.y + other.y)

    def dist(to: Coord): Long =
      math.abs(to.x - coord.x) + math.abs(to.y - coord.y)

    def distance: Long = math.max(math.max(math.abs(coord.x), math.abs(coord.y)), math.abs(coord.x - coord.y))

    def neighbours(directions: List[Direction] = Direction.values.toList): List[Coord] =
      directions.map(coord + _.direction)

enum Direction(val direction: Coord) extends Enum[Direction]:
  case Up        extends Direction(Coord(0L, 1L))
  case UpRight   extends Direction(Coord(1L, 1L))
  case Right     extends Direction(Coord(1L, 0L))
  case DownRight extends Direction(Coord(1L, -1L))
  case Down      extends Direction(Coord(0L, -1L))
  case DownLeft  extends Direction(Coord(-1L, -1L))
  case Left      extends Direction(Coord(-1L, 0L))
  case UpLeft    extends Direction(Coord(-1L, 1L))

  def oposite: Direction = this match
    case Up        => Down
    case UpRight   => DownLeft
    case Right     => Left
    case DownRight => UpLeft
    case Down      => Up
    case DownLeft  => UpRight
    case Left      => Right
    case UpLeft    => DownRight

object Direction:
  val hvOnly: List[Direction] = List(Up, Right, Down, Left)
