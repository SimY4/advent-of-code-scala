package aoc.y2017

import scala.annotation.tailrec

object Day6:
  def solve(input: String): Int =
    val memoryBanks = "\\d+".r.findAllIn(input).map(_.toInt).toVector

    @tailrec def loop(memoryBanks: Vector[Int], states: Map[Vector[Int], Int] = Map.empty): Int =
      if states.contains(memoryBanks) then states.size
      else
        val maxMemoryBankIdx = memoryBanks.indices.maxBy(memoryBanks)
        val memoryBankBlock  = memoryBanks(maxMemoryBankIdx)

        val newMemoryBank = (for
          i <- 1 to memoryBankBlock
          pos = (maxMemoryBankIdx + i) % memoryBanks.size
        yield pos)
          .foldLeft(memoryBanks.updated(maxMemoryBankIdx, 0)): (banks, i) =>
            banks.updated(i, banks(i) + 1)

        loop(newMemoryBank, states.updated(memoryBanks, states.size))

    loop(memoryBanks)

  def solve2(input: String): Int =
    val memoryBanks = "\\d+".r.findAllIn(input).map(_.toInt).toVector

    @tailrec def loop(memoryBanks: Vector[Int], states: Map[Vector[Int], Int] = Map.empty): Int =
      if states.contains(memoryBanks) then states.size - states(memoryBanks)
      else
        val maxMemoryBankIdx = memoryBanks.indices.maxBy(memoryBanks)
        val memoryBankBlock  = memoryBanks(maxMemoryBankIdx)

        val newMemoryBank = (for
          i <- 1 to memoryBankBlock
          pos = (maxMemoryBankIdx + i) % memoryBanks.size
        yield pos)
          .foldLeft(memoryBanks.updated(maxMemoryBankIdx, 0)): (banks, i) =>
            banks.updated(i, banks(i) + 1)

        loop(newMemoryBank, states.updated(memoryBanks, states.size))

    loop(memoryBanks)

  val input = "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"
