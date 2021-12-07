package aoc.y2018

import scala.annotation.tailrec
import scala.collection.immutable.{ SortedMap, SortedSet }

object Day7:
  final private case class Arrow(from: String, to: String)

  private def arrows(input: String): List[Arrow] =
    input.linesIterator.map { line =>
      "[A-Z]".r.findAllIn(line).toList match
        case _ :: from :: to :: Nil => Arrow(from, to)
    }.toList

  private def graph(arrows: List[Arrow]): Map[String, SortedSet[String]] =
    val steps = (arrows.map(_.from) ++ arrows.map(_.to)).zip(LazyList.continually(SortedSet.empty[String])).toMap
    arrows.foldLeft(steps) { (map, arr) =>
      map + (arr.to -> (map(arr.to) + arr.from))
    }

  def solve(input: String): String =
    @tailrec def solve0(reqs: Map[String, SortedSet[String]], res: String): String =
      if reqs.isEmpty then res
      else
        val step = reqs.filter((_, set) => set.isEmpty).map(_._1).toList.sorted.head
        val newReqs =
          for (k, set) <- reqs
          yield k -> (set - step)
        solve0(newReqs - step, res + step)

    solve0(graph(arrows(input)), "")

  def solve2(input: String): Long =
    def duration(step: String): Long = step.head.toLong - 'A'.toLong + 61

    def work(queue: SortedMap[String, Long]): SortedMap[String, Long] =
      queue.foldLeft(SortedMap.empty[String, Long]) { (q, e) =>
        q + (e._1 -> (e._2 - 1))
      }

    @tailrec def solve0(reqs: Map[String, SortedSet[String]], queue: SortedMap[String, Long], time: Long): Long =
      if reqs.isEmpty && queue.isEmpty then time
      else
        queue.filter((_, time) => time == 0L).map(_._1).toList.headOption match
          case Some(step) =>
            val newReqs =
              for (k, set) <- reqs
              yield k -> (set - step)
            solve0(newReqs - step, queue - step, time)
          case None =>
            if queue.size >= 5 then solve0(reqs, work(queue), time + 1)
            else
              reqs.filter((_, set) => set.isEmpty).map(_._1).toList.sorted.headOption match
                case Some(step) =>
                  solve0(reqs - step, queue + (step -> duration(step)), time)
                case None =>
                  solve0(reqs, work(queue), time + 1)

    solve0(graph(arrows(input)), SortedMap.empty, 0)

  val input = """Step S must be finished before step B can begin.
                |Step B must be finished before step Y can begin.
                |Step R must be finished before step E can begin.
                |Step H must be finished before step M can begin.
                |Step C must be finished before step F can begin.
                |Step K must be finished before step A can begin.
                |Step V must be finished before step W can begin.
                |Step W must be finished before step L can begin.
                |Step J must be finished before step L can begin.
                |Step Q must be finished before step A can begin.
                |Step U must be finished before step L can begin.
                |Step Y must be finished before step M can begin.
                |Step T must be finished before step F can begin.
                |Step D must be finished before step A can begin.
                |Step I must be finished before step M can begin.
                |Step O must be finished before step P can begin.
                |Step A must be finished before step L can begin.
                |Step P must be finished before step N can begin.
                |Step X must be finished before step Z can begin.
                |Step G must be finished before step N can begin.
                |Step M must be finished before step F can begin.
                |Step N must be finished before step L can begin.
                |Step F must be finished before step Z can begin.
                |Step Z must be finished before step E can begin.
                |Step E must be finished before step L can begin.
                |Step O must be finished before step X can begin.
                |Step B must be finished before step V can begin.
                |Step H must be finished before step Q can begin.
                |Step T must be finished before step M can begin.
                |Step A must be finished before step G can begin.
                |Step R must be finished before step H can begin.
                |Step S must be finished before step C can begin.
                |Step N must be finished before step Z can begin.
                |Step Z must be finished before step L can begin.
                |Step Q must be finished before step Z can begin.
                |Step R must be finished before step G can begin.
                |Step P must be finished before step Z can begin.
                |Step U must be finished before step M can begin.
                |Step W must be finished before step D can begin.
                |Step F must be finished before step L can begin.
                |Step D must be finished before step P can begin.
                |Step I must be finished before step E can begin.
                |Step M must be finished before step E can begin.
                |Step H must be finished before step N can begin.
                |Step F must be finished before step E can begin.
                |Step D must be finished before step L can begin.
                |Step C must be finished before step E can begin.
                |Step H must be finished before step Z can begin.
                |Step W must be finished before step Q can begin.
                |Step X must be finished before step E can begin.
                |Step G must be finished before step M can begin.
                |Step X must be finished before step M can begin.
                |Step Y must be finished before step P can begin.
                |Step S must be finished before step I can begin.
                |Step P must be finished before step X can begin.
                |Step S must be finished before step T can begin.
                |Step I must be finished before step N can begin.
                |Step P must be finished before step L can begin.
                |Step C must be finished before step X can begin.
                |Step I must be finished before step G can begin.
                |Step O must be finished before step F can begin.
                |Step I must be finished before step X can begin.
                |Step C must be finished before step Z can begin.
                |Step B must be finished before step K can begin.
                |Step T must be finished before step P can begin.
                |Step Q must be finished before step X can begin.
                |Step M must be finished before step N can begin.
                |Step H must be finished before step O can begin.
                |Step Q must be finished before step M can begin.
                |Step U must be finished before step F can begin.
                |Step Y must be finished before step O can begin.
                |Step D must be finished before step O can begin.
                |Step R must be finished before step T can begin.
                |Step A must be finished before step E can begin.
                |Step A must be finished before step M can begin.
                |Step C must be finished before step N can begin.
                |Step G must be finished before step E can begin.
                |Step C must be finished before step Y can begin.
                |Step A must be finished before step Z can begin.
                |Step S must be finished before step X can begin.
                |Step V must be finished before step Z can begin.
                |Step Q must be finished before step I can begin.
                |Step P must be finished before step E can begin.
                |Step D must be finished before step F can begin.
                |Step M must be finished before step Z can begin.
                |Step U must be finished before step N can begin.
                |Step Q must be finished before step L can begin.
                |Step O must be finished before step Z can begin.
                |Step N must be finished before step E can begin.
                |Step S must be finished before step W can begin.
                |Step S must be finished before step O can begin.
                |Step U must be finished before step T can begin.
                |Step A must be finished before step P can begin.
                |Step J must be finished before step I can begin.
                |Step A must be finished before step F can begin.
                |Step U must be finished before step D can begin.
                |Step W must be finished before step X can begin.
                |Step O must be finished before step L can begin.
                |Step J must be finished before step D can begin.
                |Step R must be finished before step Z can begin.
                |Step O must be finished before step N can begin.""".stripMargin
