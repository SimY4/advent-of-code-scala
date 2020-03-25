package aoc.y2018

import scala.annotation.tailrec
import scala.collection.immutable.{ SortedMap, SortedSet }

object Day7 {
  private final case class Arrow(from: String, to: String)

  private def arrows(input: String): List[Arrow] =
    input.linesIterator.map { line =>
      "[A-Z]".r.findAllIn(line).toList match {
        case _ :: from :: to :: Nil => Arrow(from, to)
        case _ => ???
      }
    }.toList

  private def graph(arrows: List[Arrow]): Map[String, SortedSet[String]] = {
    val steps = (arrows.map(_.from) ++ arrows.map(_.to)).zip(LazyList.continually(SortedSet.empty[String])).toMap
    arrows.foldLeft(steps) { (map, arr) =>
      map + (arr.to -> (map(arr.to) + arr.from))
    }
  }

  def solve(input: String): String = {
    @tailrec def solve0(reqs: Map[String, SortedSet[String]], res: String): String =
      if (reqs.isEmpty) res
      else {
        val step = reqs.filter((_, set) => set.isEmpty).map(_._1).toList.sorted.head
        val newReqs = for {
          (k, set) <- reqs
        } yield k -> (set - step)
        solve0(newReqs - step, res + step)
      }

    solve0(graph(arrows(input)), "")
  }

  def solve2(input: String): Long = {
    def duration(step: String): Long = step.head.toLong - 'A'.toLong + 61

    def work(queue: SortedMap[String, Long]): SortedMap[String, Long] =
      queue.foldLeft(SortedMap.empty[String, Long]) { (q, e) =>
        q + (e._1 -> (e._2 - 1))
      }

    @tailrec def solve0(reqs: Map[String, SortedSet[String]], queue: SortedMap[String, Long], time: Long): Long =
      if (reqs.isEmpty && queue.isEmpty) time
      else {
        queue.filter((_, time) => time == 0L).map(_._1).toList.headOption match {
          case Some(step) =>
            val newReqs = 
              for ((k, set) <- reqs)
              yield k -> (set - step)
            solve0(newReqs - step, queue - step, time)
          case None =>
            if (queue.size >= 5) solve0(reqs, work(queue), time + 1)
            else reqs.filter((_, set) => set.isEmpty).map(_._1).toList.sorted.headOption match {
              case Some(step) =>
                solve0(reqs - step, queue + (step -> duration(step)), time)
              case None =>
                solve0(reqs, work(queue), time + 1)
            }
        }
      }

    solve0(graph(arrows(input)), SortedMap.empty, 0)
  }

  val input = """Step C must be finished before step A can begin.
                |Step C must be finished before step F can begin.
                |Step A must be finished before step B can begin.
                |Step A must be finished before step D can begin.
                |Step B must be finished before step E can begin.
                |Step D must be finished before step E can begin.
                |Step F must be finished before step E can begin.""".stripMargin
}
