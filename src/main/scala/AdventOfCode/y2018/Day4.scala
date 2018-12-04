package AdventOfCode
package y2018

import java.text.SimpleDateFormat
import java.time.{ Duration, Instant, LocalDateTime, ZoneOffset }

import scala.annotation.tailrec

object Day4 {

  val input = """[1518-11-01 00:00] Guard #10 begins shift
                |[1518-11-01 00:05] falls asleep
                |[1518-11-01 00:25] wakes up
                |[1518-11-01 00:30] falls asleep
                |[1518-11-01 00:55] wakes up
                |[1518-11-01 23:58] Guard #99 begins shift
                |[1518-11-02 00:40] falls asleep
                |[1518-11-02 00:50] wakes up
                |[1518-11-03 00:05] Guard #10 begins shift
                |[1518-11-03 00:24] falls asleep
                |[1518-11-03 00:29] wakes up
                |[1518-11-04 00:02] Guard #99 begins shift
                |[1518-11-04 00:36] falls asleep
                |[1518-11-04 00:46] wakes up
                |[1518-11-05 00:03] Guard #99 begins shift
                |[1518-11-05 00:45] falls asleep
                |[1518-11-05 00:55] wakes up""".stripMargin

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  case class Record(ts: Instant, action: Action)
  sealed trait Action
  case class BeginsDuty(guard: Int) extends Action
  case object FallsAsleep extends Action
  case object WakesUp extends Action
  case class SleepSchedule(sleepStart: LocalDateTime, sleepEnd: LocalDateTime)

  def schedule(input: String): Map[Int, List[SleepSchedule]] = {
    def parse(line: String): Record = {
      val matcher = raw"\[(?<ts>\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (?<ac>.+)".r.pattern.matcher(line)
      matcher.find
      Record(dateFormat.parse(matcher.group("ts")).toInstant, matcher.group("ac") match {
        case s if s.startsWith("wakes up") => WakesUp
        case s if s.startsWith("falls asleep") => FallsAsleep
        case s if s.startsWith("Guard #") => "\\d+".r.findFirstIn(s).map { g => BeginsDuty(g.toInt) }.get
      })
    }
    implicit def toLocalDateTime(i: Instant): LocalDateTime = LocalDateTime.ofInstant(i, ZoneOffset.UTC)
    def schedule0(records: List[Record], current: Int, schedule: Map[Int, List[SleepSchedule]]): Map[Int, List[SleepSchedule]] = {
      records match {
        case Nil => schedule
        case Record(_, BeginsDuty(d)) :: rs => schedule0(rs, d, schedule)
        case Record(start, FallsAsleep) :: Record(end, WakesUp) :: rs => schedule0(rs, current, 
          schedule + (current -> (schedule.getOrElse(current, Nil) ++ List(SleepSchedule(start, end)))))
      }
    }

    schedule0(input.linesIterator
      .map(parse)
      .toList
      .sortBy(_.ts), -1, Map.empty)
  }

  case class State(curentGuard: Int, sleepCount: Map[Int, Int], overlaps: Map[Int, Set[Instant]])

  def solve(input: String): Int = {
    val sched = schedule(input)
    val (id, longestSleeperSchedule) = sched.maxBy { case (_, list) => list.map { s => Duration.between(s.sleepStart, s.sleepEnd) }.reduce(_.plus(_)) }
    val maxMinuteSleeping = (for {
      SleepSchedule(start, end) <- sched(id)
      minute <- start.getMinute until end.getMinute
    } yield minute)
      .groupBy(identity)
      .maxBy { case (_, list) =>  list.size }
      ._1
    id * maxMinuteSleeping
  }

  def solve2(input: String): Int = {
    val sched = schedule(input)
    val (id, (maxMinuteSleeping, _)) = sched.mapValues { list => (for {
        SleepSchedule(start, end) <- list
        minute <- start.getMinute until end.getMinute
      } yield minute)
        .groupBy(identity)
        .mapValues(_.size)
        .maxBy { case (_, size) => size }
    }.maxBy { case (_, (min, count)) => count }
    println(id)
    println(maxMinuteSleeping)
    id * maxMinuteSleeping
  }
  

}