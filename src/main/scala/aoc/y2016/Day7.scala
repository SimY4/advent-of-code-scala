package aoc
package y2016

object Day7 extends Input(2016, 7):
  private val aba  = "(?=((.)(?!\\2).\\2))".r
  private val abba = "(.)(?!\\1)(.)\\2\\1".r

  private def bab(aba: String): String = s"${aba.charAt(1)}${aba.charAt(0)}${aba.charAt(1)}"

  def solve(input: String): Int =
    (for
      line <- input.linesIterator
      hypernet = line.split("\\[[^\\]]+]")
      if hypernet.exists(abba.findFirstIn(_).isDefined)
      tls = line.substring(line.indexOf('[') + 1, line.lastIndexOf(']')).split("][^\\[]+\\[")
      if !tls.exists(abba.findFirstIn(_).isDefined)
    yield line).size

  def solve2(input: String): Int =
    (for
      line <- input.linesIterator
      hypernet = line.split("\\[[^\\]]+]")
      tls      = line.substring(line.indexOf('[') + 1, line.lastIndexOf(']')).split("][^\\[]+\\[")
      if hypernet.exists(aba.findAllMatchIn(_).map(m => bab(m.group(1))).exists(bab => tls.exists(_.contains(bab))))
    yield line).size
