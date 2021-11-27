package aoc.y2016

import scala.annotation.tailrec

object Day16:
  def solve(input: String, length: Int = 272): String =
    LazyList
      .iterate(input) { data =>
        val a = data
        val b = a.reverse.map {
          case '0' => '1'
          case '1' => '0'
        }
        a + "0" + b
      }
      .dropWhile(_.size < length)
      .map(_.substring(0, length))
      .map { data =>
        @tailrec def checksum(d: String): String =
          val cs = d
            .sliding(2, 2)
            .map { pair =>
              if pair(0) == pair(1) then '1'
              else '0'
            }
            .mkString
          if (cs.length & 1) == 0 then checksum(cs)
          else cs
        checksum(data)
      }
      .head

  def solve2(input: String): String = solve(input, 35651584)

  val input = "01111001100111011"
