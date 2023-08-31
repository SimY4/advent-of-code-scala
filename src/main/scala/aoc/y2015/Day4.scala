package aoc
package y2015

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day4:
  private val md = MessageDigest.getInstance("MD5")

  def solve(input: String, prefix: String = "00000"): Option[Int] = LazyList
    .from(1)
    .find: i =>
      md.update((input + i).getBytes(StandardCharsets.UTF_8))
      val hex = md.digest().printHexBinary
      hex.startsWith(prefix)

  def solve2(input: String): Option[Int] = solve(input, "000000")

  val input = "bgvyzdsv"
