package aoc.y2017

object Day15:
  private def generator(factor: Long, starts: Long): LazyList[Long] =
    LazyList.iterate(starts)(n => (n * factor) % 2147483647L)

  def solve(genA: Long, genB: Long): Int =
    generator(16807L, genA)
      .zip(generator(48271L, genB))
      .take(40000000)
      .count((a, b) => (a & 0xffff) == (b & 0xffff))

  def solve2(genA: Long, genB: Long): Int =
    generator(16807L, genA)
      .filter(i => i % 4L == 0L)
      .zip(generator(48271L, genB).filter(i => i % 8L == 0L))
      .take(5000000)
      .count((a, b) => (a & 0xffff) == (b & 0xffff))

  val input = 722L -> 354L
