package aoc
package y2019

object Day3:
  private def wire(line: String): Vector[Coord] =
    (for
      instruction <- line.split(",").toVector
      (d, m) = instruction.splitAt(1)
      direction <- Vector.fill(m.toInt):
        d match
          case "R" => Direction.Right
          case "U" => Direction.Up
          case "L" => Direction.Left
          case "D" => Direction.Down
    yield direction)
      .scanLeft(Coord(0, 0))(_ + _.direction)

  def solve(input: String): Long =
    (for
      line <- input.linesIterator
      wire <- wire(line).drop(1).distinct.map((_, 1))
    yield wire).toVector
      .groupMap(_(0))(_(1))
      .filter(_(1).size >= 2)
      .map:
        case (Coord(x, y), _) => math.abs(x) + math.abs(y)
      .min

  def solve2(input: String): Int =
    (for
      line <- input.linesIterator
      wire <- wire(line).zipWithIndex.drop(1).distinctBy(_(0))
    yield wire).toVector
      .groupMap(_(0))(_(1))
      .values
      .filter(_.size >= 2)
      .map(_.sum)
      .min

  val input =
    """R990,U796,R784,U604,R6,U437,L96,U285,L361,U285,L339,D512,L389,D840,L425,U444,L485,D528,L262,U178,L80,U2,R952,U459,L361,D985,R56,U135,R953,D913,L361,U120,L329,U965,L294,U890,L126,U214,R232,D444,L714,U791,R888,U923,R378,U233,L654,D703,R902,D715,R469,D60,R990,U238,R755,U413,L409,D601,R452,U504,R472,D874,L766,D594,R696,U398,R593,D889,R609,D405,L962,U176,L237,U642,L393,D91,L463,U936,R199,D136,R601,D8,R359,D863,L410,U598,L444,D34,R664,D323,R72,D98,L565,D476,L197,D132,R510,U665,R936,U3,R385,U144,L284,D713,L605,U106,R543,D112,R528,D117,R762,U330,R722,U459,L229,U375,L870,D81,R623,U95,L148,D530,L622,D62,R644,D365,L214,U847,R31,D832,L648,D293,R79,D748,L270,U159,L8,U83,R195,U912,L409,D649,L750,D286,L623,D956,R81,U775,R44,D437,L199,U698,L42,U419,L883,U636,L323,U89,L246,D269,L992,U739,R62,U47,R63,U17,L234,U135,R126,D208,L69,U550,L123,D66,R463,U992,R411,D276,L851,U520,R805,D300,L894,U171,L922,D901,R637,U907,R328,U433,L316,D644,L398,U10,L648,D190,R884,U474,R397,D718,L925,D578,R249,U959,L697,D836,R231,U806,R982,U827,R579,U830,L135,D666,R818,D502,L898,D585,R91,D190,L255,U535,R56,U390,R619,D815,L300,D81,R432,D70,L940,D587,L259,D196,R241,U4,R440,U678,R185,U451,R733,D984,R464,D298,L738,U600,R353,D44,L458,U559,L726,D786,L307,D333,L226,D463,R138,D142,L521,D201,R51,D202,L204,U130,L333,U597,R298,U42,L951,U66,R312,U707,L555,D225,L360,D12,L956,D361,L989,D625,L944,D398,L171,D982,L377,U114,L339,U164,R39,D793,R992,U834,R675,U958,R334,D697,L734,D40,L149,U394,R976
      |L1005,D52,L125,U787,L761,U262,L466,D966,R895,U789,R6,U2,R870,U971,R238,D946,L752,D240,R721,U349,L679,D518,L104,U417,L462,U544,L519,U797,R873,U70,R298,U45,L779,D921,R468,D421,R803,U108,L812,D498,R226,D309,R766,U724,L961,U472,R940,U944,R418,D682,R328,U55,R737,U961,L343,U397,R112,D292,L155,U162,R398,U445,L524,U256,R323,D587,L862,D726,R624,D230,R460,U539,R723,U93,L507,U608,L150,U159,R35,U458,R208,U546,L495,D835,L636,U960,L322,U408,L78,D250,L994,U818,R107,U978,R401,D147,R574,D549,R983,U698,L99,D63,L772,U409,R975,U990,L893,U467,L860,D721,R504,U102,R678,D672,L406,D933,R743,D788,R142,D44,R208,D424,R28,D674,R331,D968,L154,U206,R222,D354,R687,D331,L539,D390,L373,D514,L622,U673,R345,U943,L508,D337,R265,D785,L189,U429,R344,D719,R622,U199,L765,U350,R833,U309,R95,U911,R548,U746,R107,D867,L648,D680,R28,U596,L891,U168,R933,U571,R365,U267,R916,D130,L149,D898,L513,D167,R587,U799,R134,D328,R562,D929,L399,U568,R565,U241,L395,U822,L624,D145,L995,U516,R474,D609,R153,U52,R561,D15,R283,U321,L850,U218,L225,D635,L630,U102,L84,D672,L128,D885,L506,U911,R355,D132,R155,D120,L110,U368,R149,D343,L708,U378,R591,D585,L381,D517,R852,U432,R342,U273,R893,D277,L548,U859,L891,U311,L901,U255,R421,U90,L72,D474,L654,U12,L146,D867,L485,D663,R123,D82,L21,U408,L38,D864,L114,D645,R936,U765,L832,D668,L482,U79,L594,U276,L559,D469,R314,D162,R621,U230,L688,U82,R605,U191,L381,D327,L91,D217,R714,D942,R851,U358,R22,U952,R279,D897,R485,D867,L940,U891,L223,D264,L634,D560,R645,D705,L289,U584,R97,U920,R41""".stripMargin
