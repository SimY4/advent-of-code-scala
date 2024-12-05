package aoc

import java.net.URI
import scala.util.Using

trait Input(year: Int, day: Int):
  lazy val input: String =
    val url        = URI.create(s"https://adventofcode.com/$year/day/$day/input").toURL
    val connection = url.openConnection()
    connection.addRequestProperty("Cookie", s"session=${Input.SESSION}")
    Using.resource(connection.getInputStream)(scala.io.Source.fromInputStream(_).mkString)

object Input:
  private val SESSION: String = System.getProperty("aoc.session", "SESSION")
