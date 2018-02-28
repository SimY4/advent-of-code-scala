package adventCode

object Day4 {

  private val passphrases = """aa bb cc dd ee
                              |aa bb cc dd aa
                              |aa bb cc dd aaa""".stripMargin

  def countValidPassphrases(passphrases: String): Int = (for {
    passphrase <- passphrases.lines
    arr = passphrase.split("\\s+")
    if arr.length == arr.toSet.size
  } yield 1).sum

  println(countValidPassphrases(passphrases) == 2)

  // PART 2

  private val passphrases2 = """abcde fghij
                               |abcde xyz ecdab
                               |a ab abc abd abf abj
                               |iiii oiii ooii oooi oooo
                               |oiii ioii iioi iiio""".stripMargin

  def countValidPassphrases2(passphrases: String): Int = (for {
    passphrase <- passphrases.lines
    arr = passphrase.split("\\s+")
    if arr.forall { w1 =>
      arr.filter(_ ne w1).forall { w2 => !(w1.toCharArray.sorted sameElements w2.toCharArray.sorted)}
    }
    _ = println(passphrase)
  } yield 1).sum

  println(countValidPassphrases2(passphrases2) == 3)

}
