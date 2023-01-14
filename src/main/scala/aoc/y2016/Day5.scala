package aoc
package y2016

import scala.collection.immutable.SortedMap

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day5:
  private val md = MessageDigest.getInstance("MD5")

  def solve(input: String): String =
    (for
      i <- LazyList.from(0)
      hex = {
        md.update((input + i).getBytes(StandardCharsets.UTF_8))
        md.digest().printHexBinary
      }
      if hex.startsWith("00000")
    yield hex.charAt(5))
      .take(8)
      .mkString

  def solve2(input: String): Option[String] =
    (for
      i <- LazyList.from(0)
      hex = {
        md.update((input + i).getBytes(StandardCharsets.UTF_8))
        md.digest().printHexBinary
      }
      if hex.startsWith("00000") && '0' <= hex.charAt(5) && hex.charAt(5) <= '7'
    yield hex.charAt(5).asDigit -> hex.charAt(6))
      .scanLeft(SortedMap.empty[Int, Char]) { case (map, (k, v)) =>
        if map.contains(k) then map
        else map.updated(k, v)
      }
      .find(_.size == 8)
      .map(_.values.mkString)

  val input = "ugkcyxxp"
