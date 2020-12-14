package aoc.y2020

object Day13 {
  def solve(input: String): Int = 
    input.linesIterator.toList match {
      case ts :: buses :: Nil => 
        val (bus, minTs) = buses.split(',').flatMap(_.toIntOption)
          .map { bus =>
            bus -> LazyList.from(ts.toInt)
              .find(_ % bus == 0)
              .get
          }
          .minBy(_._2)
        bus * (minTs - ts.toInt)
    }

  def solve2(input: String): Option[Long] = 
    input.linesIterator.toList match {
      case _ :: buses :: Nil => 
        val parsedBusses = buses.split(',').flatMap(_.toLongOption)
        val res = buses.split(',').zipWithIndex.flatMap((b, i) => b.toLongOption.map(_ - i))

        def egcd(a: Long, b: Long): (Long, Long, Long) =
          if (a == 0L) (b, 0L, 1L)
          else {
            val (g, x, y) = egcd(b % a, a)
            (g, y - (b / a) * x, x)
          }

        def modInv(x: Long, n: Long): Option[Long] = {
          val (g, h, _) = egcd(x, n)
          Option.when(g == 1L)((h % n + n) % n)
        }

        def chineseRemainder(residues: Array[Long], modulii: Array[Long]): Option[Long] = {
          val prod = modulii.product
          val sum = residues.zip(modulii)
            .map { (residue, modulus) =>
              val p = prod / modulus
              modInv(p, modulus).map(m => residue * m * p)
            }
            .reduce {
              case (Some(m1), Some(m2)) => Some(m1 + m2)
              case _ => None
            }
          sum.map(_ % prod)
        }

        chineseRemainder(res, parsedBusses)
    }

  val input = """1000104
                |41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,659,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,937,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17""".stripMargin
}