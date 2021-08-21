package aoc.y2016

import java.nio.file.Files
import java.nio.file.Paths

object Day7 {
  import scala.jdk.CollectionConverters.*

  private def abba(s: Seq[Char]): Boolean = s(0) != s(1) && s == s.reverse

  private def bab(aba: Seq[Char]): Seq[Char] = List(aba(1), aba(0), aba(1))

  def solve(input: Iterable[String]): Int =
    (for
      line    <- input
      hypernet = line.split("\\[[^\\]]+]")
      tls      = line.substring(line.indexOf('[') + 1, line.lastIndexOf(']')).split("][^\\[]+\\[")
      if hypernet.exists(_.toSeq.sliding(4).exists(abba))
      if !tls.exists(_.toSeq.sliding(4).exists(abba))
    yield ()).size

  def solve2(input: Iterable[String]): Int =
    (for
      line    <- input.view
      hypernet = line.split("\\[[^\\]]+]")
      tls      = line.substring(line.indexOf('[') + 1, line.lastIndexOf(']')).split("][^\\[]+\\[")
      if (
        for
          seg <- hypernet
          aba <- seg.toSeq.sliding(3)
          if abba(aba)
          _    = println(s"$aba -> ${bab(aba)}")
        yield bab(aba)
      ).exists(bab => tls.exists(_.toSeq.sliding(3).exists(_ == bab)))
    yield ()).size

  def input = Files.readAllLines(Paths.get(getClass.getResource("/aoc/y2016/day7-input.txt").toURI)).asScala
}
