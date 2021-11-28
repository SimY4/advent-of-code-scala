package aoc
package y2016

object Day7 extends Input(2016, 7):
  private def abba(s: Seq[Char]): Boolean = s(0) != s(1) && s == s.reverse

  private def bab(aba: Seq[Char]): Seq[Char] = List(aba(1), aba(0), aba(1))

  def solve(input: String): Int =
    (for
      line <- input.linesIterator
      hypernet = line.split("\\[[^\\]]+]")
      tls      = line.substring(line.indexOf('[') + 1, line.lastIndexOf(']')).split("][^\\[]+\\[")
      if hypernet.exists(_.toSeq.sliding(4).exists(abba))
      if !tls.exists(_.toSeq.sliding(4).exists(abba))
    yield ()).size

  def solve2(input: String): Int =
    (for
      line <- input.linesIterator
      hypernet = line.split("\\[[^\\]]+]")
      tls      = line.substring(line.indexOf('[') + 1, line.lastIndexOf(']')).split("][^\\[]+\\[")
      if (
        for
          seg <- hypernet
          aba <- seg.toSeq.sliding(3)
          if abba(aba)
        yield bab(aba)
      ).exists(bab => tls.exists(_.toSeq.sliding(3).exists(_ == bab)))
    yield ()).size
