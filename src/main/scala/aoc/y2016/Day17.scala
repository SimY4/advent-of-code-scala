package aoc
package y2016

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day17 {
  private val md = MessageDigest.getInstance("MD5")
  
  type Direction = Coord.Direction & Coord.HVDirection

  private def paths(current: Coord, path: List[Direction] = Nil): List[List[Direction]] = 
    if (Coord(3L, 0L) == current) List(path)
    else {
      md.update((input + path.reverse.map {
        case Coord.Direction.Up => 'U'
        case Coord.Direction.Right => 'R'
        case Coord.Direction.Down => 'D'
        case Coord.Direction.Left => 'L'
      }.mkString).getBytes(StandardCharsets.UTF_8))
      val hex = md.digest().printHexBinary

      val directions = for {
        d <- Coord.Direction.hvOnly
        next = current + d.direction
        if 0 <= next.x && next.x <= 3 && 0 <= next.y && next.y <= 3
        if "BCDEF".toSet.contains(d match {
          case Coord.Direction.Up => hex(0)
          case Coord.Direction.Down => hex(1)
          case Coord.Direction.Left => hex(2)
          case Coord.Direction.Right => hex(3)
        })
      } yield d

      directions.flatMap(d => paths(current + d.direction, d :: path))
    }

  def solve(input: String): List[Direction] =
    paths(Coord(0L, 3L)).minBy(_.size).reverse

  def solve2(input: String): Int =
    paths(Coord(0L, 3L)).maxBy(_.size).size

  val input = "pvhmgsws"
}