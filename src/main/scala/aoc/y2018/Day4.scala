package aoc
package y2018

import java.text.SimpleDateFormat
import java.time.{ Duration, Instant, LocalDateTime, ZoneOffset }

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day4 {
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  final private case class Record(ts: Instant, action: Action)
  private enum Action {
    case BeginsDuty(guard: Int)
    case FallsAsleep
    case WakesUp
  }

  import Action.*

  final private case class SleepSchedule(sleepStart: LocalDateTime, sleepEnd: LocalDateTime)

  extension (s: SleepSchedule) private def durationBetween: Duration = Duration.between(s.sleepStart, s.sleepEnd)
  extension (s: SleepSchedule) private def minutes: Seq[Int]         = s.sleepStart.getMinute until s.sleepEnd.getMinute

  private def schedule(input: String): Map[Int, List[SleepSchedule]] = {
    def parse(line: String): Record = {
      val matcher = raw"\[(?<ts>\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (?<ac>.+)".r.pattern.matcher(line)
      matcher.find
      Record(
        dateFormat.parse(matcher.group("ts")).toInstant,
        matcher.group("ac") match {
          case s if s.startsWith("wakes up")     => WakesUp
          case s if s.startsWith("falls asleep") => FallsAsleep
          case s if s.startsWith("Guard #")      =>
            "\\d+".r
              .findFirstIn(s)
              .map { g =>
                BeginsDuty(g.toInt)
              }
              .get
        }
      )
    }

    implicit def toLocalDateTime(i: Instant): LocalDateTime = LocalDateTime.ofInstant(i, ZoneOffset.UTC)

    @tailrec def schedule0(
      records: List[Record],
      current: Int,
      schedule: Map[Int, List[SleepSchedule]]
    ): Map[Int, List[SleepSchedule]] =
      records match {
        case Nil                                                      => schedule
        case Record(_, BeginsDuty(d)) :: rs                           => schedule0(rs, d, schedule)
        case Record(start, FallsAsleep) :: Record(end, WakesUp) :: rs =>
          schedule0(
            rs,
            current,
            schedule + (current -> (schedule.getOrElse(current, Nil) ++ List(SleepSchedule(start, end))))
          )
      }

    schedule0(
      input.linesIterator
        .map(parse)
        .toList
        .sortBy(_.ts),
      -1,
      Map.empty
    )
  }

  def solve(input: String): Int =
    val sched                        = schedule(input)
    val (id, longestSleeperSchedule) = sched.maxBy((_, list) => list.map(_.durationBetween).reduce(_.plus(_)))
    val maxMinuteSleeping            = (for
      s      <- sched(id)
      minute <- s.minutes
    yield minute)
      .groupBy(identity)
      .maxBy((_, list) => list.size)
      ._1
    id * maxMinuteSleeping

  def solve2(input: String): Int =
    val sched                        = schedule(input)
    val (id, (maxMinuteSleeping, _)) = sched.view.mapValues { list =>
      (for
        s      <- list
        minute <- s.minutes
      yield minute)
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .maxBy((_, size) => size)
    }.maxBy { case (_, (min, count)) => count }
    id * maxMinuteSleeping

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
}
