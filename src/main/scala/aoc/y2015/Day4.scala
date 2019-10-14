package aoc.y2015

object Day4
  private val hexArray = "0123456789ABCDEF".toCharArray()
  private def (bytes: Array[Byte]) printHexBinary: String =
    val hexChars = new Array[Char](bytes.length * 2)
    for (i <- 0 until bytes.length) {
      val v = bytes(i) & 0xFF
      hexChars(i * 2) = hexArray(v >>> 4)
      hexChars(i * 2 + 1) = hexArray(v & 0x0F)
    }
    new String(hexChars)

  def solve(input: String, prefix: String = "00000"): Option[Int] = Stream.from(1)
    .find { i => 
      val md = java.security.MessageDigest.getInstance("MD5")
      md.update((input + i).getBytes("UTF-8"))
      val hex = md.digest().printHexBinary
      hex.startsWith(prefix) 
    }

  def solve2(input: String): Option[Int] = solve(input, "000000")
