package aoc

import aoc.y2015
import aoc.y2016
import aoc.y2017
import aoc.y2019
import aoc.y2020
import aoc.y2022
import aoc.y2023
import aoc.y2024

final case class Timed[R](result: R, time: Long):
  override def toString: String = s"$result, time: ${time / 1000000}ms"

@main def run(): Unit =
  y2015.run()
  y2016.run()
  y2017.run()
  y2019.run()
  y2020.run()
  y2022.run()
  y2023.run()
  y2024.run()

def timed[R](block: => R): Timed[R] =
  val t0     = System.nanoTime()
  val result = block
  val t1     = System.nanoTime()
  Timed(result, t1 - t0)
