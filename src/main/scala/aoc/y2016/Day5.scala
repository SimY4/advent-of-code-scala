package aoc
package y2016

import scala.collection.immutable.SortedMap

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day5 {
  private val md = MessageDigest.getInstance("MD5")

  def solve(input: String): String = 
    (for {
      i <- LazyList.from(0)
      hex = {
        md.update((input + i).getBytes(StandardCharsets.UTF_8))
        md.digest().printHexBinary
      }
      if hex.startsWith("00000")
    } yield hex(5))
      .take(8)
      .mkString

  def solve2(input: String): Option[String] = 
    (for {
      i <- LazyList.from(0)
      hex = {
        md.update((input + i).getBytes(StandardCharsets.UTF_8))
        md.digest().printHexBinary
      }
      if hex.startsWith("00000") && '0' <= hex(5) && hex(5) <= '7'
    } yield hex(5).asDigit -> hex(6))
      .scanLeft(SortedMap.empty[Int, Char]) { (map, pair) =>
        if (map.contains(pair._1)) map
        else map.updated(pair._1, pair._2)
      }
      .find(_.size == 8)
      .map(_.values.mkString)
  
  val input = "ugkcyxxp"
}
