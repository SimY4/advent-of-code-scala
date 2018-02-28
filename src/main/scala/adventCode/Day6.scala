package adventCode

import scala.annotation.tailrec

object Day6 {

  type StepAndLoop = (Int, Int)

  private val memoryBanks = "0 2 7 0".split("\\s+").map(_.toInt).toList

  def redistribute(memoryBanks: List[Int]): StepAndLoop = {
    @tailrec def redistribute0(states: Map[List[Int], Int], memoryBanks: List[Int]): StepAndLoop = {
      if (states contains memoryBanks)
        states.size -> (states.size - states(memoryBanks))
      else {
        val maxMemoryBankIdx = memoryBanks.indices.maxBy(memoryBanks)
        val memoryBankBlock = memoryBanks(maxMemoryBankIdx)

        val newMemoryBank = (for {
          i <- 1 to memoryBankBlock
          pos = (maxMemoryBankIdx + i) % memoryBanks.size
        } yield pos)
          .foldLeft(memoryBanks.updated(maxMemoryBankIdx, 0)) { (banks, i) =>
            banks.updated(i, banks(i) + 1)
          }

        redistribute0(states ++ Map(memoryBanks -> states.size), newMemoryBank)
      }
    }

    redistribute0(Map.empty, memoryBanks)
  }

  println(redistribute(memoryBanks))

}
