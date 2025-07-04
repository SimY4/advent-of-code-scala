package aoc.y2015

object Day16:
  private enum Dogs extends Enum[Dogs]:
    case Samoyeds, Pomeranians, Akitas, Vizslas

  final private case class AuntSue(
    children: Option[Int] = None,
    cats: Option[Int] = None,
    dogs: Map[Dogs, Int] = Map.empty,
    goldfish: Option[Int] = None,
    trees: Option[Int] = None,
    cars: Option[Int] = None,
    perfumes: Option[Int] = None
  )

  private val auntSue = AuntSue(
    children = Some(3),
    cats = Some(7),
    dogs = Map(
      Dogs.Samoyeds    -> 2,
      Dogs.Pomeranians -> 3,
      Dogs.Akitas      -> 0,
      Dogs.Vizslas     -> 0
    ),
    goldfish = Some(5),
    trees = Some(3),
    cars = Some(2),
    perfumes = Some(1)
  )

  private def parseLine(line: String): AuntSue =
    line
      .split(" ")
      .toList
      .drop(2)
      .grouped(2)
      .foldLeft(AuntSue()): (sue, pair) =>
        pair match
          case "children:" :: num :: Nil    => sue.copy(children = num.replace(",", "").toIntOption)
          case "cats:" :: num :: Nil        => sue.copy(cats = num.replace(",", "").toIntOption)
          case "samoyeds:" :: num :: Nil    => sue.copy(dogs = sue.dogs + (Dogs.Samoyeds -> num.replace(",", "").toInt))
          case "pomeranians:" :: num :: Nil =>
            sue.copy(dogs = sue.dogs + (Dogs.Pomeranians -> num.replace(",", "").toInt))
          case "akitas:" :: num :: Nil   => sue.copy(dogs = sue.dogs + (Dogs.Akitas -> num.replace(",", "").toInt))
          case "vizslas:" :: num :: Nil  => sue.copy(dogs = sue.dogs + (Dogs.Vizslas -> num.replace(",", "").toInt))
          case "goldfish:" :: num :: Nil => sue.copy(goldfish = num.replace(",", "").toIntOption)
          case "trees:" :: num :: Nil    => sue.copy(trees = num.replace(",", "").toIntOption)
          case "cars:" :: num :: Nil     => sue.copy(cars = num.replace(",", "").toIntOption)
          case "perfumes:" :: num :: Nil => sue.copy(perfumes = num.replace(",", "").toIntOption)
          case _                         => sue

  def solve(input: String): Int =
    def equiv(sue: AuntSue): Boolean =
      (for
        a <- Some(sue)
        if auntSue.children.forall(lc => a.children.forall(rc => lc == rc))
        if auntSue.cats.forall(lc => a.cats.forall(rc => lc == rc))
        if (auntSue.dogs.keySet ++ a.dogs.keySet).forall: dog =>
          auntSue.dogs.get(dog).forall(ld => a.dogs.get(dog).forall(rd => ld == rd))
        if auntSue.goldfish.forall(lg => a.goldfish.forall(rg => lg == rg))
        if auntSue.trees.forall(lt => a.trees.forall(rt => lt == rt))
        if auntSue.cars.forall(lc => a.cars.forall(rc => lc == rc))
        if auntSue.perfumes.forall(lp => a.perfumes.forall(rp => lp == rp))
      yield ()).isDefined

    (for
      (line, idx) <- input.linesIterator.toSeq.zipWithIndex
      sue = parseLine(line)
      if equiv(sue)
    yield idx + 1).head

  def solve2(input: String): Int =
    def equiv(sue: AuntSue): Boolean =
      (for
        a <- Some(sue)
        if auntSue.children.forall(lc => a.children.forall(rc => lc == rc))
        if auntSue.cats.forall(lc => a.cats.forall(rc => lc < rc))
        if (auntSue.dogs.keySet ++ a.dogs.keySet)
          .forall: dog =>
            auntSue.dogs
              .get(dog)
              .forall: ld =>
                a.dogs
                  .get(dog)
                  .forall: rd =>
                    dog match
                      case Dogs.Pomeranians => ld > rd
                      case _                => ld == rd
        if auntSue.goldfish.forall(lg => a.goldfish.forall(rg => lg > rg))
        if auntSue.trees.forall(lt => a.trees.forall(rt => lt < rt))
        if auntSue.cars.forall(lc => a.cars.forall(rc => lc == rc))
        if auntSue.perfumes.forall(lp => a.perfumes.forall(rp => lp == rp))
      yield ()).isDefined

    (for
      (line, idx) <- input.linesIterator.toSeq.zipWithIndex
      sue = parseLine(line)
      if equiv(sue)
    yield idx + 1).head

  val input = """Sue 1: goldfish: 6, trees: 9, akitas: 0
                |Sue 2: goldfish: 7, trees: 1, akitas: 0
                |Sue 3: cars: 10, akitas: 6, perfumes: 7
                |Sue 4: perfumes: 2, vizslas: 0, cars: 6
                |Sue 5: goldfish: 1, trees: 3, perfumes: 10
                |Sue 6: children: 9, vizslas: 7, cars: 9
                |Sue 7: cars: 6, vizslas: 5, cats: 3
                |Sue 8: akitas: 10, vizslas: 9, children: 3
                |Sue 9: vizslas: 8, cats: 2, trees: 1
                |Sue 10: perfumes: 10, trees: 6, cars: 4
                |Sue 11: cars: 9, children: 1, cats: 1
                |Sue 12: pomeranians: 4, akitas: 6, goldfish: 8
                |Sue 13: cats: 10, children: 5, trees: 9
                |Sue 14: perfumes: 8, vizslas: 3, samoyeds: 1
                |Sue 15: vizslas: 2, perfumes: 8, trees: 3
                |Sue 16: pomeranians: 10, trees: 9, samoyeds: 4
                |Sue 17: akitas: 7, vizslas: 0, goldfish: 6
                |Sue 18: trees: 5, vizslas: 9, cars: 0
                |Sue 19: akitas: 3, goldfish: 9, trees: 10
                |Sue 20: perfumes: 7, samoyeds: 3, vizslas: 10
                |Sue 21: perfumes: 7, pomeranians: 10, akitas: 8
                |Sue 22: vizslas: 6, trees: 8, akitas: 10
                |Sue 23: goldfish: 0, trees: 4, children: 9
                |Sue 24: goldfish: 7, pomeranians: 9, akitas: 4
                |Sue 25: cars: 7, trees: 4, pomeranians: 4
                |Sue 26: trees: 9, akitas: 9, pomeranians: 7
                |Sue 27: samoyeds: 0, perfumes: 9, goldfish: 10
                |Sue 28: cars: 5, trees: 7, vizslas: 1
                |Sue 29: perfumes: 9, trees: 1, children: 6
                |Sue 30: goldfish: 10, trees: 0, cars: 4
                |Sue 31: akitas: 2, perfumes: 5, goldfish: 5
                |Sue 32: goldfish: 0, akitas: 5, trees: 0
                |Sue 33: vizslas: 2, akitas: 2, samoyeds: 3
                |Sue 34: goldfish: 8, perfumes: 5, cars: 3
                |Sue 35: akitas: 1, cats: 4, trees: 9
                |Sue 36: cars: 4, vizslas: 4, goldfish: 7
                |Sue 37: akitas: 5, perfumes: 7, trees: 3
                |Sue 38: goldfish: 10, trees: 2, vizslas: 9
                |Sue 39: goldfish: 4, pomeranians: 5, vizslas: 5
                |Sue 40: perfumes: 5, samoyeds: 4, akitas: 6
                |Sue 41: goldfish: 9, cars: 4, perfumes: 5
                |Sue 42: trees: 6, pomeranians: 9, goldfish: 8
                |Sue 43: perfumes: 7, pomeranians: 1, akitas: 2
                |Sue 44: vizslas: 9, cars: 5, cats: 0
                |Sue 45: akitas: 1, goldfish: 6, trees: 0
                |Sue 46: akitas: 5, vizslas: 8, trees: 2
                |Sue 47: trees: 9, akitas: 2, vizslas: 9
                |Sue 48: goldfish: 10, trees: 5, akitas: 2
                |Sue 49: cars: 7, vizslas: 2, perfumes: 6
                |Sue 50: akitas: 5, goldfish: 6, perfumes: 0
                |Sue 51: cars: 9, cats: 7, trees: 5
                |Sue 52: akitas: 7, goldfish: 10, cars: 0
                |Sue 53: cars: 10, cats: 4, perfumes: 2
                |Sue 54: goldfish: 2, pomeranians: 5, perfumes: 10
                |Sue 55: vizslas: 5, akitas: 4, cars: 8
                |Sue 56: goldfish: 9, vizslas: 4, akitas: 5
                |Sue 57: perfumes: 8, samoyeds: 7, cars: 9
                |Sue 58: cars: 5, akitas: 7, perfumes: 8
                |Sue 59: samoyeds: 8, cars: 10, vizslas: 10
                |Sue 60: akitas: 6, samoyeds: 0, goldfish: 3
                |Sue 61: trees: 8, pomeranians: 0, akitas: 2
                |Sue 62: trees: 1, perfumes: 3, vizslas: 4
                |Sue 63: vizslas: 6, samoyeds: 9, goldfish: 8
                |Sue 64: goldfish: 7, trees: 6, vizslas: 3
                |Sue 65: cars: 1, vizslas: 0, akitas: 6
                |Sue 66: cats: 6, pomeranians: 4, cars: 9
                |Sue 67: trees: 10, pomeranians: 7, samoyeds: 3
                |Sue 68: pomeranians: 5, goldfish: 9, akitas: 1
                |Sue 69: akitas: 1, vizslas: 0, trees: 9
                |Sue 70: cats: 4, goldfish: 4, vizslas: 10
                |Sue 71: vizslas: 7, perfumes: 7, trees: 8
                |Sue 72: children: 2, vizslas: 9, cats: 3
                |Sue 73: cars: 8, pomeranians: 0, perfumes: 6
                |Sue 74: akitas: 1, pomeranians: 8, vizslas: 10
                |Sue 75: vizslas: 5, perfumes: 5, cars: 7
                |Sue 76: cars: 3, vizslas: 3, goldfish: 0
                |Sue 77: akitas: 9, samoyeds: 1, pomeranians: 3
                |Sue 78: trees: 0, vizslas: 0, akitas: 6
                |Sue 79: pomeranians: 9, cars: 1, perfumes: 0
                |Sue 80: perfumes: 10, trees: 1, cats: 0
                |Sue 81: goldfish: 5, akitas: 9, trees: 0
                |Sue 82: vizslas: 1, akitas: 6, children: 4
                |Sue 83: samoyeds: 7, perfumes: 8, pomeranians: 4
                |Sue 84: perfumes: 3, children: 3, cats: 7
                |Sue 85: goldfish: 9, trees: 3, cars: 9
                |Sue 86: cars: 0, perfumes: 9, vizslas: 0
                |Sue 87: children: 3, trees: 4, akitas: 3
                |Sue 88: trees: 1, samoyeds: 1, goldfish: 0
                |Sue 89: akitas: 8, cars: 3, vizslas: 9
                |Sue 90: pomeranians: 9, trees: 9, goldfish: 8
                |Sue 91: goldfish: 7, trees: 10, children: 0
                |Sue 92: cats: 9, cars: 7, perfumes: 7
                |Sue 93: vizslas: 2, goldfish: 7, cats: 9
                |Sue 94: akitas: 5, cars: 8, vizslas: 4
                |Sue 95: goldfish: 7, vizslas: 1, perfumes: 2
                |Sue 96: goldfish: 5, trees: 6, perfumes: 10
                |Sue 97: trees: 0, perfumes: 7, cars: 0
                |Sue 98: cars: 2, perfumes: 6, trees: 8
                |Sue 99: trees: 10, children: 7, cats: 9
                |Sue 100: samoyeds: 5, goldfish: 6, vizslas: 6
                |Sue 101: cars: 10, perfumes: 9, vizslas: 3
                |Sue 102: pomeranians: 6, trees: 1, samoyeds: 4
                |Sue 103: cars: 2, perfumes: 1, goldfish: 5
                |Sue 104: goldfish: 2, cars: 8, pomeranians: 2
                |Sue 105: goldfish: 6, vizslas: 0, trees: 10
                |Sue 106: trees: 10, akitas: 10, pomeranians: 0
                |Sue 107: vizslas: 2, pomeranians: 10, trees: 3
                |Sue 108: children: 3, vizslas: 8, akitas: 7
                |Sue 109: perfumes: 2, akitas: 2, samoyeds: 3
                |Sue 110: goldfish: 7, trees: 1, perfumes: 1
                |Sue 111: akitas: 2, cars: 9, perfumes: 2
                |Sue 112: children: 10, cars: 0, akitas: 3
                |Sue 113: akitas: 9, vizslas: 4, children: 3
                |Sue 114: pomeranians: 3, trees: 2, goldfish: 5
                |Sue 115: perfumes: 8, cars: 6, trees: 0
                |Sue 116: samoyeds: 6, children: 3, pomeranians: 1
                |Sue 117: goldfish: 1, trees: 2, akitas: 1
                |Sue 118: goldfish: 10, akitas: 10, samoyeds: 0
                |Sue 119: vizslas: 10, perfumes: 6, cars: 0
                |Sue 120: cars: 2, perfumes: 9, goldfish: 5
                |Sue 121: vizslas: 2, trees: 2, cars: 6
                |Sue 122: vizslas: 3, trees: 0, akitas: 2
                |Sue 123: akitas: 5, samoyeds: 7, goldfish: 1
                |Sue 124: goldfish: 8, samoyeds: 7, trees: 8
                |Sue 125: trees: 3, goldfish: 8, perfumes: 5
                |Sue 126: cats: 3, vizslas: 9, goldfish: 0
                |Sue 127: pomeranians: 9, goldfish: 3, perfumes: 6
                |Sue 128: vizslas: 4, cars: 8, goldfish: 5
                |Sue 129: vizslas: 8, children: 5, perfumes: 8
                |Sue 130: cars: 7, children: 7, cats: 3
                |Sue 131: perfumes: 1, akitas: 8, vizslas: 9
                |Sue 132: perfumes: 7, samoyeds: 10, pomeranians: 6
                |Sue 133: cars: 5, perfumes: 3, goldfish: 7
                |Sue 134: perfumes: 9, akitas: 2, cats: 3
                |Sue 135: perfumes: 1, trees: 9, vizslas: 9
                |Sue 136: akitas: 7, cars: 3, perfumes: 7
                |Sue 137: vizslas: 9, goldfish: 8, cars: 5
                |Sue 138: trees: 0, samoyeds: 1, cars: 3
                |Sue 139: cars: 0, perfumes: 6, trees: 0
                |Sue 140: pomeranians: 4, cars: 1, perfumes: 7
                |Sue 141: vizslas: 10, akitas: 8, cats: 3
                |Sue 142: trees: 1, cats: 6, vizslas: 5
                |Sue 143: pomeranians: 9, cars: 7, perfumes: 9
                |Sue 144: cars: 0, perfumes: 2, pomeranians: 1
                |Sue 145: trees: 1, goldfish: 9, perfumes: 8
                |Sue 146: cars: 8, children: 5, vizslas: 2
                |Sue 147: perfumes: 2, goldfish: 5, cars: 0
                |Sue 148: akitas: 2, perfumes: 7, pomeranians: 6
                |Sue 149: goldfish: 8, cars: 0, trees: 1
                |Sue 150: akitas: 6, perfumes: 5, trees: 0
                |Sue 151: vizslas: 6, samoyeds: 8, akitas: 10
                |Sue 152: trees: 7, akitas: 7, perfumes: 6
                |Sue 153: goldfish: 9, cats: 9, cars: 3
                |Sue 154: vizslas: 10, trees: 0, cars: 9
                |Sue 155: perfumes: 3, children: 2, goldfish: 1
                |Sue 156: goldfish: 7, perfumes: 5, akitas: 6
                |Sue 157: cats: 10, trees: 1, goldfish: 0
                |Sue 158: cats: 7, children: 7, vizslas: 6
                |Sue 159: perfumes: 9, akitas: 0, cars: 0
                |Sue 160: akitas: 3, goldfish: 10, pomeranians: 2
                |Sue 161: goldfish: 10, cars: 6, perfumes: 3
                |Sue 162: trees: 0, cars: 9, goldfish: 1
                |Sue 163: cars: 8, perfumes: 9, vizslas: 5
                |Sue 164: goldfish: 1, trees: 10, children: 6
                |Sue 165: goldfish: 0, vizslas: 6, cars: 0
                |Sue 166: akitas: 5, vizslas: 1, cars: 5
                |Sue 167: vizslas: 1, samoyeds: 1, children: 4
                |Sue 168: samoyeds: 7, vizslas: 7, akitas: 3
                |Sue 169: goldfish: 3, cats: 9, trees: 2
                |Sue 170: cars: 5, perfumes: 9, vizslas: 5
                |Sue 171: goldfish: 7, cars: 6, perfumes: 10
                |Sue 172: cats: 6, akitas: 1, children: 6
                |Sue 173: cats: 4, goldfish: 1, children: 3
                |Sue 174: cars: 2, pomeranians: 2, vizslas: 7
                |Sue 175: trees: 0, children: 4, goldfish: 7
                |Sue 176: children: 8, cars: 5, cats: 9
                |Sue 177: pomeranians: 4, vizslas: 7, trees: 3
                |Sue 178: vizslas: 6, perfumes: 10, akitas: 6
                |Sue 179: cars: 4, akitas: 4, trees: 4
                |Sue 180: akitas: 8, goldfish: 6, trees: 9
                |Sue 181: perfumes: 3, vizslas: 10, cars: 3
                |Sue 182: vizslas: 3, samoyeds: 3, goldfish: 7
                |Sue 183: goldfish: 10, perfumes: 2, cats: 1
                |Sue 184: goldfish: 5, trees: 1, perfumes: 1
                |Sue 185: vizslas: 10, trees: 9, perfumes: 2
                |Sue 186: goldfish: 6, perfumes: 9, trees: 1
                |Sue 187: cars: 0, trees: 9, goldfish: 6
                |Sue 188: cars: 0, trees: 1, vizslas: 9
                |Sue 189: akitas: 7, vizslas: 2, trees: 0
                |Sue 190: pomeranians: 5, perfumes: 8, akitas: 10
                |Sue 191: vizslas: 5, akitas: 3, cats: 0
                |Sue 192: children: 1, trees: 1, cars: 2
                |Sue 193: cars: 3, goldfish: 9, trees: 2
                |Sue 194: samoyeds: 3, akitas: 4, perfumes: 8
                |Sue 195: trees: 1, vizslas: 8, akitas: 10
                |Sue 196: akitas: 6, cars: 5, pomeranians: 0
                |Sue 197: akitas: 5, vizslas: 5, cats: 1
                |Sue 198: trees: 4, cars: 6, goldfish: 6
                |Sue 199: cats: 7, cars: 5, goldfish: 6
                |Sue 200: vizslas: 4, cats: 0, akitas: 9
                |Sue 201: pomeranians: 1, perfumes: 4, children: 2
                |Sue 202: cats: 1, perfumes: 4, vizslas: 3
                |Sue 203: vizslas: 1, akitas: 9, children: 5
                |Sue 204: perfumes: 8, cars: 7, trees: 4
                |Sue 205: perfumes: 7, pomeranians: 5, cats: 9
                |Sue 206: vizslas: 8, trees: 2, akitas: 2
                |Sue 207: akitas: 6, vizslas: 2, perfumes: 10
                |Sue 208: vizslas: 1, children: 7, akitas: 4
                |Sue 209: perfumes: 4, trees: 2, children: 1
                |Sue 210: goldfish: 0, vizslas: 2, samoyeds: 10
                |Sue 211: cars: 8, perfumes: 3, trees: 1
                |Sue 212: cars: 8, samoyeds: 5, pomeranians: 8
                |Sue 213: akitas: 2, goldfish: 8, pomeranians: 2
                |Sue 214: akitas: 6, pomeranians: 2, cars: 0
                |Sue 215: trees: 10, pomeranians: 4, vizslas: 0
                |Sue 216: perfumes: 0, cars: 8, trees: 0
                |Sue 217: samoyeds: 8, akitas: 7, children: 10
                |Sue 218: perfumes: 1, vizslas: 6, children: 0
                |Sue 219: children: 1, goldfish: 4, trees: 1
                |Sue 220: akitas: 10, goldfish: 10, trees: 5
                |Sue 221: cars: 7, pomeranians: 6, perfumes: 3
                |Sue 222: vizslas: 6, children: 0, akitas: 5
                |Sue 223: perfumes: 9, cars: 1, trees: 6
                |Sue 224: pomeranians: 1, trees: 0, vizslas: 0
                |Sue 225: goldfish: 8, akitas: 4, perfumes: 10
                |Sue 226: pomeranians: 7, cats: 7, children: 4
                |Sue 227: trees: 0, akitas: 2, perfumes: 1
                |Sue 228: vizslas: 6, cars: 10, perfumes: 9
                |Sue 229: cars: 0, perfumes: 6, trees: 4
                |Sue 230: pomeranians: 7, perfumes: 5, trees: 2
                |Sue 231: goldfish: 9, cars: 6, trees: 7
                |Sue 232: akitas: 1, vizslas: 5, cars: 3
                |Sue 233: akitas: 7, samoyeds: 2, vizslas: 5
                |Sue 234: akitas: 6, cats: 8, pomeranians: 0
                |Sue 235: pomeranians: 5, akitas: 5, vizslas: 3
                |Sue 236: goldfish: 5, trees: 6, akitas: 5
                |Sue 237: goldfish: 9, perfumes: 5, cats: 5
                |Sue 238: cats: 8, goldfish: 4, perfumes: 0
                |Sue 239: samoyeds: 8, children: 6, pomeranians: 6
                |Sue 240: akitas: 4, samoyeds: 10, trees: 8
                |Sue 241: trees: 2, goldfish: 8, cars: 1
                |Sue 242: perfumes: 2, cars: 0, akitas: 10
                |Sue 243: pomeranians: 1, cars: 7, trees: 2
                |Sue 244: trees: 9, vizslas: 2, akitas: 10
                |Sue 245: cars: 9, pomeranians: 4, trees: 0
                |Sue 246: cars: 9, pomeranians: 7, perfumes: 1
                |Sue 247: trees: 0, goldfish: 1, akitas: 8
                |Sue 248: vizslas: 1, cats: 4, akitas: 4
                |Sue 249: cats: 6, children: 4, goldfish: 9
                |Sue 250: vizslas: 1, cars: 10, samoyeds: 5
                |Sue 251: cars: 0, goldfish: 1, vizslas: 7
                |Sue 252: cars: 7, akitas: 9, vizslas: 10
                |Sue 253: akitas: 7, vizslas: 2, perfumes: 5
                |Sue 254: vizslas: 10, akitas: 5, samoyeds: 0
                |Sue 255: pomeranians: 8, goldfish: 0, cats: 6
                |Sue 256: cars: 10, goldfish: 8, vizslas: 9
                |Sue 257: goldfish: 3, perfumes: 9, cats: 3
                |Sue 258: trees: 6, goldfish: 6, cars: 6
                |Sue 259: trees: 0, goldfish: 2, perfumes: 8
                |Sue 260: trees: 5, akitas: 0, cars: 0
                |Sue 261: pomeranians: 9, goldfish: 7, perfumes: 8
                |Sue 262: perfumes: 8, vizslas: 6, goldfish: 2
                |Sue 263: vizslas: 6, trees: 5, goldfish: 9
                |Sue 264: vizslas: 4, perfumes: 7, cars: 9
                |Sue 265: goldfish: 10, trees: 3, perfumes: 1
                |Sue 266: trees: 10, akitas: 8, goldfish: 8
                |Sue 267: goldfish: 4, trees: 0, samoyeds: 9
                |Sue 268: vizslas: 1, trees: 0, goldfish: 8
                |Sue 269: cars: 2, perfumes: 10, goldfish: 5
                |Sue 270: perfumes: 7, cars: 2, vizslas: 1
                |Sue 271: cars: 6, perfumes: 10, goldfish: 6
                |Sue 272: samoyeds: 4, goldfish: 2, vizslas: 9
                |Sue 273: perfumes: 4, goldfish: 4, vizslas: 1
                |Sue 274: children: 4, cars: 4, perfumes: 3
                |Sue 275: children: 8, vizslas: 3, trees: 2
                |Sue 276: vizslas: 5, children: 7, perfumes: 3
                |Sue 277: perfumes: 3, cats: 4, vizslas: 5
                |Sue 278: cars: 1, samoyeds: 10, akitas: 2
                |Sue 279: trees: 9, perfumes: 9, cars: 10
                |Sue 280: vizslas: 5, trees: 0, perfumes: 6
                |Sue 281: vizslas: 3, akitas: 10, pomeranians: 7
                |Sue 282: trees: 1, children: 2, akitas: 8
                |Sue 283: akitas: 9, goldfish: 6, cats: 5
                |Sue 284: cars: 9, children: 10, pomeranians: 2
                |Sue 285: pomeranians: 0, perfumes: 4, cars: 7
                |Sue 286: perfumes: 0, vizslas: 10, akitas: 10
                |Sue 287: cats: 2, perfumes: 3, trees: 5
                |Sue 288: akitas: 9, vizslas: 8, samoyeds: 9
                |Sue 289: perfumes: 6, children: 2, cars: 7
                |Sue 290: akitas: 0, children: 5, cars: 5
                |Sue 291: cars: 4, perfumes: 0, trees: 1
                |Sue 292: cats: 0, cars: 8, perfumes: 6
                |Sue 293: akitas: 9, cats: 5, children: 5
                |Sue 294: akitas: 4, cars: 9, goldfish: 3
                |Sue 295: cars: 2, akitas: 3, perfumes: 7
                |Sue 296: perfumes: 4, cars: 7, goldfish: 10
                |Sue 297: trees: 5, akitas: 8, vizslas: 1
                |Sue 298: perfumes: 0, goldfish: 6, trees: 9
                |Sue 299: perfumes: 6, samoyeds: 8, cars: 1
                |Sue 300: goldfish: 10, perfumes: 4, akitas: 2
                |Sue 301: cars: 3, trees: 0, goldfish: 8
                |Sue 302: perfumes: 7, samoyeds: 2, vizslas: 7
                |Sue 303: children: 10, goldfish: 7, perfumes: 2
                |Sue 304: samoyeds: 8, vizslas: 2, cars: 1
                |Sue 305: trees: 1, cats: 0, goldfish: 10
                |Sue 306: trees: 4, perfumes: 2, cars: 7
                |Sue 307: cars: 6, vizslas: 2, children: 6
                |Sue 308: vizslas: 2, cars: 0, akitas: 7
                |Sue 309: cars: 3, vizslas: 8, perfumes: 6
                |Sue 310: goldfish: 7, perfumes: 7, vizslas: 3
                |Sue 311: pomeranians: 10, trees: 2, cars: 0
                |Sue 312: samoyeds: 2, vizslas: 9, akitas: 1
                |Sue 313: cars: 4, pomeranians: 7, goldfish: 7
                |Sue 314: akitas: 2, pomeranians: 9, samoyeds: 10
                |Sue 315: akitas: 3, vizslas: 2, trees: 0
                |Sue 316: cars: 0, perfumes: 4, pomeranians: 6
                |Sue 317: akitas: 10, goldfish: 3, pomeranians: 7
                |Sue 318: cars: 9, trees: 0, pomeranians: 9
                |Sue 319: akitas: 3, vizslas: 7, children: 10
                |Sue 320: vizslas: 0, akitas: 8, pomeranians: 4
                |Sue 321: cars: 10, akitas: 9, vizslas: 3
                |Sue 322: perfumes: 0, akitas: 8, vizslas: 6
                |Sue 323: vizslas: 10, perfumes: 5, cars: 3
                |Sue 324: akitas: 0, goldfish: 6, vizslas: 7
                |Sue 325: perfumes: 9, vizslas: 5, pomeranians: 2
                |Sue 326: vizslas: 6, goldfish: 10, pomeranians: 8
                |Sue 327: vizslas: 10, cars: 1, akitas: 7
                |Sue 328: trees: 1, perfumes: 10, cars: 10
                |Sue 329: pomeranians: 5, samoyeds: 3, cars: 10
                |Sue 330: akitas: 6, cars: 1, pomeranians: 4
                |Sue 331: cars: 5, children: 2, trees: 0
                |Sue 332: vizslas: 6, pomeranians: 1, perfumes: 0
                |Sue 333: akitas: 7, trees: 1, cats: 9
                |Sue 334: vizslas: 6, goldfish: 9, akitas: 7
                |Sue 335: akitas: 3, samoyeds: 3, cars: 3
                |Sue 336: samoyeds: 10, perfumes: 9, trees: 6
                |Sue 337: vizslas: 2, cars: 9, akitas: 0
                |Sue 338: akitas: 6, perfumes: 9, vizslas: 3
                |Sue 339: cars: 3, samoyeds: 8, trees: 2
                |Sue 340: cats: 7, perfumes: 8, cars: 9
                |Sue 341: goldfish: 9, perfumes: 5, cars: 10
                |Sue 342: trees: 0, akitas: 3, perfumes: 5
                |Sue 343: perfumes: 2, children: 0, cars: 6
                |Sue 344: goldfish: 8, trees: 8, perfumes: 0
                |Sue 345: perfumes: 6, cars: 6, goldfish: 5
                |Sue 346: vizslas: 8, trees: 1, cars: 6
                |Sue 347: cars: 0, cats: 3, perfumes: 7
                |Sue 348: children: 7, perfumes: 10, cars: 7
                |Sue 349: pomeranians: 8, akitas: 5, children: 2
                |Sue 350: perfumes: 9, pomeranians: 4, goldfish: 3
                |Sue 351: perfumes: 8, pomeranians: 7, trees: 4
                |Sue 352: samoyeds: 1, goldfish: 9, akitas: 8
                |Sue 353: akitas: 6, goldfish: 10, vizslas: 8
                |Sue 354: akitas: 7, cars: 2, goldfish: 6
                |Sue 355: cars: 3, goldfish: 6, akitas: 5
                |Sue 356: akitas: 2, goldfish: 9, pomeranians: 1
                |Sue 357: goldfish: 10, cars: 6, pomeranians: 9
                |Sue 358: trees: 0, children: 2, goldfish: 6
                |Sue 359: samoyeds: 3, cars: 2, akitas: 4
                |Sue 360: trees: 1, goldfish: 8, cars: 5
                |Sue 361: akitas: 5, vizslas: 7, perfumes: 1
                |Sue 362: cats: 5, vizslas: 9, children: 4
                |Sue 363: goldfish: 9, perfumes: 3, vizslas: 9
                |Sue 364: children: 7, samoyeds: 2, pomeranians: 10
                |Sue 365: perfumes: 9, akitas: 10, pomeranians: 4
                |Sue 366: cars: 10, trees: 3, cats: 4
                |Sue 367: vizslas: 6, akitas: 10, perfumes: 5
                |Sue 368: akitas: 9, vizslas: 9, children: 4
                |Sue 369: goldfish: 8, trees: 2, perfumes: 5
                |Sue 370: trees: 0, children: 4, cars: 8
                |Sue 371: cats: 6, perfumes: 0, vizslas: 2
                |Sue 372: akitas: 7, cars: 5, perfumes: 3
                |Sue 373: cars: 0, perfumes: 4, pomeranians: 10
                |Sue 374: akitas: 5, perfumes: 5, vizslas: 2
                |Sue 375: goldfish: 7, trees: 10, pomeranians: 7
                |Sue 376: cars: 8, trees: 1, pomeranians: 8
                |Sue 377: cars: 0, akitas: 9, vizslas: 1
                |Sue 378: akitas: 5, perfumes: 3, vizslas: 7
                |Sue 379: trees: 2, goldfish: 8, pomeranians: 8
                |Sue 380: akitas: 5, cars: 9, perfumes: 9
                |Sue 381: cars: 2, perfumes: 6, trees: 3
                |Sue 382: perfumes: 6, vizslas: 2, goldfish: 9
                |Sue 383: akitas: 8, vizslas: 7, cats: 1
                |Sue 384: akitas: 9, trees: 10, vizslas: 7
                |Sue 385: cars: 0, perfumes: 7, vizslas: 2
                |Sue 386: vizslas: 10, akitas: 4, perfumes: 9
                |Sue 387: perfumes: 6, pomeranians: 5, samoyeds: 8
                |Sue 388: vizslas: 10, trees: 9, goldfish: 9
                |Sue 389: goldfish: 8, akitas: 4, perfumes: 10
                |Sue 390: goldfish: 6, trees: 8, akitas: 1
                |Sue 391: vizslas: 4, akitas: 10, goldfish: 7
                |Sue 392: akitas: 1, vizslas: 6, samoyeds: 5
                |Sue 393: trees: 6, cars: 3, akitas: 5
                |Sue 394: goldfish: 9, trees: 3, cars: 5
                |Sue 395: akitas: 6, samoyeds: 4, goldfish: 4
                |Sue 396: akitas: 2, trees: 1, cats: 5
                |Sue 397: cars: 0, children: 9, trees: 10
                |Sue 398: pomeranians: 3, samoyeds: 9, goldfish: 10
                |Sue 399: cars: 7, akitas: 4, goldfish: 8
                |Sue 400: cars: 4, akitas: 5, vizslas: 4
                |Sue 401: pomeranians: 5, akitas: 8, vizslas: 5
                |Sue 402: cats: 7, cars: 6, goldfish: 6
                |Sue 403: samoyeds: 8, perfumes: 4, cars: 5
                |Sue 404: akitas: 10, goldfish: 4, trees: 2
                |Sue 405: trees: 8, perfumes: 1, cars: 2
                |Sue 406: trees: 0, perfumes: 9, pomeranians: 10
                |Sue 407: perfumes: 4, trees: 7, goldfish: 3
                |Sue 408: akitas: 1, perfumes: 3, cars: 5
                |Sue 409: trees: 6, samoyeds: 3, cars: 9
                |Sue 410: vizslas: 3, goldfish: 5, akitas: 7
                |Sue 411: goldfish: 10, trees: 1, vizslas: 9
                |Sue 412: cars: 0, akitas: 6, trees: 6
                |Sue 413: goldfish: 7, trees: 0, cars: 3
                |Sue 414: pomeranians: 10, samoyeds: 3, cars: 10
                |Sue 415: perfumes: 6, trees: 9, cars: 4
                |Sue 416: trees: 2, cars: 4, goldfish: 8
                |Sue 417: goldfish: 2, cars: 9, cats: 5
                |Sue 418: vizslas: 1, cars: 9, akitas: 0
                |Sue 419: perfumes: 6, cats: 3, children: 9
                |Sue 420: cats: 5, goldfish: 7, akitas: 9
                |Sue 421: trees: 1, samoyeds: 6, pomeranians: 1
                |Sue 422: trees: 10, goldfish: 6, children: 7
                |Sue 423: cars: 8, goldfish: 7, vizslas: 3
                |Sue 424: samoyeds: 9, akitas: 7, trees: 5
                |Sue 425: akitas: 5, children: 4, perfumes: 9
                |Sue 426: goldfish: 1, children: 9, cats: 2
                |Sue 427: vizslas: 9, akitas: 7, goldfish: 9
                |Sue 428: pomeranians: 7, akitas: 5, vizslas: 1
                |Sue 429: vizslas: 7, goldfish: 7, cars: 9
                |Sue 430: trees: 7, perfumes: 0, pomeranians: 5
                |Sue 431: children: 9, perfumes: 5, vizslas: 7
                |Sue 432: trees: 6, samoyeds: 7, cats: 1
                |Sue 433: goldfish: 5, trees: 5, children: 6
                |Sue 434: goldfish: 9, akitas: 7, cars: 3
                |Sue 435: samoyeds: 10, perfumes: 2, cars: 0
                |Sue 436: akitas: 5, pomeranians: 4, perfumes: 7
                |Sue 437: vizslas: 5, cats: 6, perfumes: 5
                |Sue 438: trees: 2, goldfish: 6, vizslas: 7
                |Sue 439: samoyeds: 8, pomeranians: 10, goldfish: 1
                |Sue 440: akitas: 6, children: 9, perfumes: 4
                |Sue 441: cars: 2, goldfish: 9, children: 0
                |Sue 442: goldfish: 7, cars: 2, vizslas: 8
                |Sue 443: goldfish: 6, samoyeds: 3, perfumes: 2
                |Sue 444: trees: 2, goldfish: 7, cars: 8
                |Sue 445: trees: 2, pomeranians: 0, children: 0
                |Sue 446: perfumes: 4, akitas: 4, goldfish: 6
                |Sue 447: vizslas: 7, akitas: 9, cars: 3
                |Sue 448: goldfish: 6, trees: 9, cars: 0
                |Sue 449: samoyeds: 7, perfumes: 4, vizslas: 10
                |Sue 450: akitas: 7, cars: 10, goldfish: 7
                |Sue 451: goldfish: 4, children: 7, pomeranians: 4
                |Sue 452: cats: 4, vizslas: 6, trees: 7
                |Sue 453: cars: 1, trees: 10, goldfish: 9
                |Sue 454: trees: 2, goldfish: 3, vizslas: 10
                |Sue 455: pomeranians: 9, vizslas: 3, akitas: 2
                |Sue 456: vizslas: 10, akitas: 2, goldfish: 1
                |Sue 457: trees: 5, cats: 5, children: 8
                |Sue 458: cars: 6, goldfish: 3, akitas: 9
                |Sue 459: goldfish: 7, akitas: 2, cats: 7
                |Sue 460: akitas: 1, cars: 5, children: 8
                |Sue 461: cars: 8, perfumes: 0, goldfish: 6
                |Sue 462: pomeranians: 6, cats: 2, perfumes: 6
                |Sue 463: vizslas: 7, perfumes: 3, goldfish: 3
                |Sue 464: akitas: 10, goldfish: 10, trees: 1
                |Sue 465: vizslas: 0, akitas: 2, trees: 2
                |Sue 466: perfumes: 6, akitas: 8, cars: 2
                |Sue 467: goldfish: 1, cars: 10, perfumes: 3
                |Sue 468: goldfish: 4, trees: 2, cars: 9
                |Sue 469: perfumes: 6, pomeranians: 0, vizslas: 10
                |Sue 470: samoyeds: 8, children: 0, akitas: 7
                |Sue 471: children: 3, goldfish: 9, cats: 9
                |Sue 472: samoyeds: 0, goldfish: 0, trees: 0
                |Sue 473: trees: 3, goldfish: 4, vizslas: 1
                |Sue 474: perfumes: 10, cars: 3, trees: 7
                |Sue 475: akitas: 5, vizslas: 4, goldfish: 5
                |Sue 476: children: 2, akitas: 7, vizslas: 3
                |Sue 477: vizslas: 6, pomeranians: 9, trees: 6
                |Sue 478: vizslas: 7, pomeranians: 6, akitas: 7
                |Sue 479: trees: 2, perfumes: 2, children: 2
                |Sue 480: cars: 8, cats: 5, vizslas: 0
                |Sue 481: trees: 5, goldfish: 0, akitas: 3
                |Sue 482: cars: 8, perfumes: 6, goldfish: 10
                |Sue 483: goldfish: 0, cars: 3, perfumes: 10
                |Sue 484: pomeranians: 1, samoyeds: 1, perfumes: 3
                |Sue 485: trees: 0, akitas: 2, vizslas: 4
                |Sue 486: cars: 3, vizslas: 8, goldfish: 1
                |Sue 487: pomeranians: 9, vizslas: 2, children: 10
                |Sue 488: akitas: 6, vizslas: 10, perfumes: 9
                |Sue 489: goldfish: 6, vizslas: 4, cars: 2
                |Sue 490: vizslas: 10, cats: 8, samoyeds: 1
                |Sue 491: cats: 9, cars: 1, perfumes: 10
                |Sue 492: goldfish: 6, cars: 9, pomeranians: 9
                |Sue 493: children: 10, goldfish: 10, vizslas: 0
                |Sue 494: pomeranians: 5, cars: 0, vizslas: 0
                |Sue 495: vizslas: 7, perfumes: 6, samoyeds: 3
                |Sue 496: trees: 1, cats: 4, cars: 10
                |Sue 497: cats: 1, perfumes: 0, cars: 7
                |Sue 498: perfumes: 7, vizslas: 6, cats: 9
                |Sue 499: vizslas: 8, perfumes: 1, akitas: 3
                |Sue 500: perfumes: 4, cars: 9, trees: 4""".stripMargin
