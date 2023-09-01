package aoc.y2015

object Day19:
  private val replacementsRegex = "(\\w+) => (\\w+)".r
  final private case class Input(replacements: List[(String, String)], molecule: String)

  private def parseInput(input: String): Input =
    val replacements = input.linesIterator
      .takeWhile(_.nonEmpty)
      .map:
        case replacementsRegex(from, to) => from -> to
      .toList
    val molecule = input.linesIterator.foldLeft(null: String)((acc, line) => line)
    Input(replacements, molecule)

  extension (replacement: (String, String))
    private def scanReplace(input: String): LazyList[String] =
      val (from, to) = replacement
      val idx        = input.indexOf(from, 0)
      if idx < 0 then LazyList.empty
      else
        LazyList
          .iterate(idx)(i => input.indexOf(from, i + 1))
          .takeWhile(0 <= _)
          .map(idx => new StringBuilder(input).replace(idx, idx + from.length, to).toString)

  def solve(input: String): Int =
    val Input(replacements, molecule) = parseInput(input)
    replacements
      .flatMap(_.scanReplace(molecule).map(_.hashCode))
      .distinct
      .size

  def solve2(input: String): Int =
    val Input(replacements, medicine) = parseInput(input)
    LazyList
      .iterate(List(medicine)): molecules =>
        (for
          (from, to)   <- replacements
          nextMolecule <- (to, from).scanReplace(molecules.head)
        yield nextMolecule).distinct
      .indexWhere(_.exists(_ == "e"))

  val input =
    """Al => ThF
      |Al => ThRnFAr
      |B => BCa
      |B => TiB
      |B => TiRnFAr
      |Ca => CaCa
      |Ca => PB
      |Ca => PRnFAr
      |Ca => SiRnFYFAr
      |Ca => SiRnMgAr
      |Ca => SiTh
      |F => CaF
      |F => PMg
      |F => SiAl
      |H => CRnAlAr
      |H => CRnFYFYFAr
      |H => CRnFYMgAr
      |H => CRnMgYFAr
      |H => HCa
      |H => NRnFYFAr
      |H => NRnMgAr
      |H => NTh
      |H => OB
      |H => ORnFAr
      |Mg => BF
      |Mg => TiMg
      |N => CRnFAr
      |N => HSi
      |O => CRnFYFAr
      |O => CRnMgAr
      |O => HP
      |O => NRnFAr
      |O => OTi
      |P => CaP
      |P => PTi
      |P => SiRnFAr
      |Si => CaSi
      |Th => ThCa
      |Ti => BP
      |Ti => TiTi
      |e => HF
      |e => NAl
      |e => OMg
      |
      |CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl""".stripMargin
