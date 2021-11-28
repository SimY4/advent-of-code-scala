package aoc

import scala.util.Using

trait Input(year: Int, day: Int):
  lazy val input: String =
    val url        = new java.net.URL(s"https://adventofcode.com/$year/day/$day/input")
    val connection = url.openConnection()
    connection.addRequestProperty("Cookie", s"session=${Input.SESSION}")
    Using.resource(connection.getInputStream) { in =>
      scala.io.Source.fromInputStream(in).getLines.mkString(System.lineSeparator)
    }

object Input:
  val SESSION = System.getProperty("aoc.session", "SESSION")
