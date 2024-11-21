package aoc

import scala.annotation.{ tailrec, targetName }

extension [A](as: List[A])
  def pairs: List[(A, A)] =
    (as.head -> as.last) :: as.zip(as.tail)

extension (n: Long)
  def factorial: Long = (1L to n).product

  def factors: Seq[Long] =
    (1L to math.sqrt(n.toDouble).toLong).flatMap: i =>
      if n % i == 0L then
        val div = n / i
        if i != div then i :: div :: Nil
        else i :: Nil
      else Nil

  @tailrec def gcd(b: Long): Long =
    if b == 0 then n else b.gcd(n % b)

  def u: Long = n << 1 >>> 1

private val hexArray = "0123456789ABCDEF".toCharArray

extension (bytes: Array[Byte])
  def printHexBinary: String =
    val hexChars = new Array[Char](bytes.length * 2)
    for i <- bytes.indices do
      val v = bytes(i) & 0xff
      hexChars(i * 2) = hexArray(v >>> 4)
      hexChars(i * 2 + 1) = hexArray(v & 0x0f)
    new String(hexChars)

final case class Coord(x: Long, y: Long):
  @targetName("plus") def +(other: Coord): Coord =
    Coord(x + other.x, y + other.y)

  def distance(to: Coord): Double =
    math.hypot((x - to.x).toDouble, (y - to.y).toDouble)

  def distance: Long = x.abs max y.abs max math.abs(x - y)

  def manhattan(to: Coord): Long =
    math.abs(to.x - x) + math.abs(to.y - y)

  def neighbours(directions: List[Direction] = Direction.values.toList): List[Coord] =
    directions.map(this + _.direction)

object Coord extends ((Long, Long) => Coord):
  def apply[I: Integral](x: I, y: I): Coord =
    import Integral.Implicits.*

    Coord(x.toLong, y.toLong)

extension [A](dd: Array[Array[A]])
  def apply(coord: Coord): A       = dd(coord.y.toInt)(coord.x.toInt)
  def get(coord: Coord): Option[A] = dd.lift(coord.y.toInt).flatMap(_.lift(coord.x.toInt))

extension (lines: Seq[String])
  def apply(coord: Coord): Char       = lines(coord.y.toInt).charAt(coord.x.toInt)
  def get(coord: Coord): Option[Char] = lines.lift(coord.y.toInt).flatMap(_.lift(coord.x.toInt))

sealed trait HV:
  def left: Direction & HV =
    this match
      case Direction.Up    => Direction.Left
      case Direction.Right => Direction.Up
      case Direction.Down  => Direction.Right
      case Direction.Left  => Direction.Down

  def right: Direction & HV =
    this match
      case Direction.Up    => Direction.Right
      case Direction.Right => Direction.Down
      case Direction.Down  => Direction.Left
      case Direction.Left  => Direction.Up

enum Direction(val direction: Coord) extends Enum[Direction]:
  case Up        extends Direction(Coord(0L, 1L)) with HV
  case UpRight   extends Direction(Coord(1L, 1L))
  case Right     extends Direction(Coord(1L, 0L)) with HV
  case DownRight extends Direction(Coord(1L, -1L))
  case Down      extends Direction(Coord(0L, -1L)) with HV
  case DownLeft  extends Direction(Coord(-1L, -1L))
  case Left      extends Direction(Coord(-1L, 0L)) with HV
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
  def hvOnly: List[Direction & HV] = List(Up, Right, Down, Left)
