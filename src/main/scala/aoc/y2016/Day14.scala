package aoc
package y2016

import scala.collection.mutable

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day14 {
  private val md = MessageDigest.getInstance("MD5")

  def solve(input: String): Int = {
    def hash(i: Int): String = {
      md.update((input + i).getBytes(StandardCharsets.UTF_8))
      md.digest().printHexBinary
    }

    LazyList.from(0)
      .filter { i => 
        hash(i).sliding(3)
          .find(_.toSet.size == 1)
          .exists { triple => 
            ((i + 1) to (i + 1001))
              .exists { j =>
                hash(j).sliding(5)
                  .filter(_.toSet.size == 1)
                  .find(_.contains(triple))
                  .isDefined
              }
          }
      }
      .drop(63)
      .head
  }

  def solve2(input: String): Int = {
    val cache = mutable.HashMap.empty[Int, String]

    def hash(i: Int): String = {
      md.update((input + i).getBytes(StandardCharsets.UTF_8))
      val hex = md.digest().printHexBinary.toLowerCase
      (1 to 2016).foldLeft(hex) { (acc, _) =>
        md.update(acc.getBytes(StandardCharsets.UTF_8))
        md.digest().printHexBinary.toLowerCase
      }
    }

    (0 to 999).foreach { i => cache(i) = hash(i) }

    LazyList.from(0)
      .filter { i => 
        val cur = cache(i % 1000)
        cache(i % 1000) = hash(i + 1000)
        cur.sliding(3)
          .find(_.toSet.size == 1)
          .exists { triple => 
            cache.values.exists { hash =>
                hash.sliding(5)
                  .filter(_.toSet.size == 1)
                  .find(_.contains(triple))
                  .isDefined
              }
          }
      }
      .drop(63)
      .head
  }

  val input = "cuanljph"
}