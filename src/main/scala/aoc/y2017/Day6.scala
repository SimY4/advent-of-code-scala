package aoc.y2017

import scala.annotation.tailrec

object Day6:
  final case class StepAndLoop(step: Int, loop: Int)

  def solve(input: String): StepAndLoop =
    val memoryBanks = "\\d+".r.findAllIn(input).map(_.toInt).toList

    @tailrec def redistribute0(states: Map[List[Int], Int], memoryBanks: List[Int]): StepAndLoop =
      if states contains memoryBanks then StepAndLoop(states.size, (states.size - states(memoryBanks)))
      else
        val maxMemoryBankIdx = memoryBanks.indices.maxBy(memoryBanks)
        val memoryBankBlock  = memoryBanks(maxMemoryBankIdx)

        val newMemoryBank = (for
          i <- 1 to memoryBankBlock
          pos = (maxMemoryBankIdx + i) % memoryBanks.size
        yield pos)
          .foldLeft(memoryBanks.updated(maxMemoryBankIdx, 0)) { (banks, i) =>
            banks.updated(i, banks(i) + 1)
          }

        redistribute0(states ++ Map(memoryBanks -> states.size), newMemoryBank)

    redistribute0(Map.empty, memoryBanks)

  val input = "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"
