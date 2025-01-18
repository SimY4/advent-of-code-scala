package aoc
package y2016

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day17:
  import Direction.*

  private val md = MessageDigest.getInstance("MD5")

  private def paths(current: Coord, path: List[Direction & HV] = Nil): List[List[Direction & HV]] =
    if Coord(3L, 0L) == current then List(path)
    else
      md.update(
        (input + path.reverse
          .map:
            case Up    => 'U'
            case Right => 'R'
            case Down  => 'D'
            case Left  => 'L'
          .mkString).getBytes(StandardCharsets.UTF_8)
      )
      val hex = md.digest().printHexBinary

      val directions = for
        d <- Direction.hvOnly
        next = current + d.direction
        if 0 <= next.x && next.x <= 3 && 0 <= next.y && next.y <= 3
        if "BCDEF".toSet.contains(d match
          case Up    => hex.charAt(0)
          case Down  => hex.charAt(1)
          case Left  => hex.charAt(2)
          case Right => hex.charAt(3))
      yield d

      directions.flatMap(d => paths(current + d.direction, d :: path))

  def solve(input: String): String =
    paths(Coord(0L, 3L)).minBy(_.size).reverse.map(_.toString.head).mkString

  def solve2(input: String): Int =
    paths(Coord(0L, 3L)).maxBy(_.size).size

  val input = "pvhmgsws"
