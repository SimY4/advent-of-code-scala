package aoc
package y2016

import scala.collection.mutable

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day14:
  private val md        = MessageDigest.getInstance("MD5")
  private val triple    = "(.)\\1\\1".r
  private val quintuple = "(.)\\1\\1\\1\\1".r

  def solve(input: String): Int =
    def hash(i: Int): String =
      md.update((input + i).getBytes(StandardCharsets.UTF_8))
      md.digest().printHexBinary

    LazyList
      .from(0)
      .filter: i =>
        val h = hash(i)
        triple
          .findFirstIn(h)
          .exists: triple =>
            ((i + 1) to (i + 1001)).exists: j =>
              quintuple.findFirstIn(hash(j)).exists(_.contains(triple))
      .drop(63)
      .head

  def solve2(input: String): Int =
    val cache = mutable.HashMap.empty[Int, String]

    def hash(i: Int): String =
      md.update((input + i).getBytes(StandardCharsets.UTF_8))
      val hex = md.digest().printHexBinary.toLowerCase
      LazyList
        .iterate(hex): acc =>
          md.update(acc.getBytes(StandardCharsets.UTF_8))
          md.digest().printHexBinary.toLowerCase
        .drop(2016)
        .head

    for i <- 0 to 999 do cache.put(i, hash(i))

    LazyList
      .from(0)
      .filter: i =>
        val cur = cache(i % 1000)
        cache.put(i % 1000, hash(i + 1000))
        triple
          .findFirstIn(cur)
          .exists: triple =>
            cache.values.exists: hash =>
              quintuple.findFirstIn(hash).exists(_.contains(triple))
      .drop(63)
      .head

  val input = "cuanljph"
