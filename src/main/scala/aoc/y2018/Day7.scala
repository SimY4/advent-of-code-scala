package aoc.y2018

import scala.annotation.tailrec
import scala.collection.immutable.{ SortedMap, SortedSet }

object Day7:
  final private case class Arrow(from: String, to: String)

  private def graph(arrows: Vector[Arrow]): Map[String, SortedSet[String]] =
    val steps = (arrows.map(_.from) ++ arrows.map(_.to)).zip(LazyList.continually(SortedSet.empty[String])).toMap
    arrows.foldLeft(steps): (map, arr) =>
      map + (arr.to -> (map(arr.to) + arr.from))

  def solve(input: String): String =
    @tailrec def loop(reqs: Map[String, SortedSet[String]], res: StringBuilder = new StringBuilder()): String =
      if reqs.isEmpty then res.toString
      else
        val step = reqs
          .collect:
            case (step, set) if set.isEmpty => step
          .toList
          .min
        val newReqs =
          for (k, set) <- reqs
          yield k -> (set - step)
        loop(newReqs - step, res.append(step))

    loop(
      graph(
        input.linesIterator
          .map:
            case s"Step $from must be finished before step $to can begin." => Arrow(from, to)
          .toVector
      )
    )

  def solve2(input: String): Long =
    def work(queue: SortedMap[String, Long]): SortedMap[String, Long] =
      queue.foldLeft(SortedMap.empty[String, Long]):
        case (q, (step, duration)) =>
          q + (step -> (duration - 1))

    val grph = graph(
      input.linesIterator
        .map:
          case s"Step $from must be finished before step $to can begin." => Arrow(from, to)
        .toVector
    )

    LazyList
      .iterate(grph -> SortedMap.empty[String, Long]): (reqs, queue) =>
        queue
          .collect:
            case (step, 0L) => step
          .toList
          .headOption match
          case Some(step) =>
            val newReqs =
              for (k, set) <- reqs
              yield k -> (set - step)
            (newReqs - step, queue - step)
          case None if queue.size >= 5 => (reqs, work(queue))
          case None                    =>
            reqs
              .collect:
                case (step, set) if set.isEmpty => step
              .toVector
              .sorted
              .headOption match
              case Some(step) => (reqs - step, queue + (step -> (step.head.toLong - 'A'.toLong + 61)))
              case None       => (reqs, work(queue))
      .indexWhere((grph, queue) => grph.isEmpty && queue.isEmpty)

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
