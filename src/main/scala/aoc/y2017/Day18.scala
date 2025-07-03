package aoc.y2017

import scala.annotation.tailrec

import java.util.concurrent.{ BlockingQueue, LinkedBlockingQueue, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger

object Day18:
  private enum Ins:
    case Snd(value: Long Either String)
    case Set(reg: String, value: Long Either String)
    case Add(reg: String, value: Long Either String)
    case Mul(reg: String, value: Long Either String)
    case Mod(reg: String, value: Long Either String)
    case Rcv(reg: String)
    case Jgz(reg: Long Either String, value: Long Either String)

  import Ins.*

  private def parseLine(line: String): Ins =
    line match
      case s"snd $x"    => Snd(x.toLongOption.toLeft(x))
      case s"set $r $x" => Set(r, x.toLongOption.toLeft(x))
      case s"add $r $x" => Add(r, x.toLongOption.toLeft(x))
      case s"mul $r $x" => Mul(r, x.toLongOption.toLeft(x))
      case s"mod $r $x" => Mod(r, x.toLongOption.toLeft(x))
      case s"rcv $x"    => Rcv(x)
      case s"jgz $r $x" => Jgz(r.toLongOption.toLeft(r), x.toLongOption.toLeft(x))

  @tailrec private def runProgram(
    program: Vector[Ins],
    state: Map[String, Long] = Map.empty.withDefaultValue(0),
    sound: Long = 0,
    cursor: Int = 0
  ): Long =
    program.lift(cursor) match
      case None                  => sound
      case Some(Snd(value))      => runProgram(program, state, value.fold(identity, state), cursor + 1)
      case Some(Set(reg, value)) =>
        runProgram(program, state.updated(reg, value.fold(identity, state)), sound, cursor + 1)
      case Some(Add(reg, value)) =>
        runProgram(program, state.updated(reg, state(reg) + value.fold(identity, state)), sound, cursor + 1)
      case Some(Mul(reg, value)) =>
        runProgram(program, state.updated(reg, state(reg) * value.fold(identity, state)), sound, cursor + 1)
      case Some(Mod(reg, value)) =>
        runProgram(program, state.updated(reg, state(reg) % value.fold(identity, state)), sound, cursor + 1)
      case Some(Rcv(reg)) =>
        if state(reg) != 0 then sound else runProgram(program, state, sound, cursor + 1)
      case Some(Jgz(reg, value)) =>
        runProgram(
          program,
          state,
          sound,
          cursor + (if reg.fold(identity, state) > 0L then value.fold(_.toInt, state(_).toInt) else 1)
        )

  def solve(input: String): Long =
    val instructions = input.linesIterator.map(parseLine).toVector

    runProgram(instructions)

  private class Agent(
    program: Vector[Ins],
    id: Long,
    rcv: BlockingQueue[Long],
    snd: BlockingQueue[Long]
  ) extends Thread(s"Agent-$id"):
    setDaemon(true)

    val sent = AtomicInteger(0)

    override def run(): Unit =
      @tailrec def runProgram(state: Map[String, Long], cursor: Int = 0): Unit =
        program.lift(cursor) match
          case None             => ()
          case Some(Snd(value)) =>
            sent.incrementAndGet()
            snd.put(value.fold(identity, state))
            runProgram(state, cursor + 1)
          case Some(Set(reg, value)) =>
            runProgram(state.updated(reg, value.fold(identity, state)), cursor + 1)
          case Some(Add(reg, value)) =>
            runProgram(state.updated(reg, state(reg) + value.fold(identity, state)), cursor + 1)
          case Some(Mul(reg, value)) =>
            runProgram(state.updated(reg, state(reg) * value.fold(identity, state)), cursor + 1)
          case Some(Mod(reg, value)) =>
            runProgram(state.updated(reg, state(reg) % value.fold(identity, state)), cursor + 1)
          case Some(Rcv(reg)) =>
            Option(rcv.poll(1, TimeUnit.SECONDS)) match
              case None        => ()
              case Some(value) => runProgram(state.updated(reg, value), cursor + 1)
          case Some(Jgz(reg, value)) =>
            runProgram(
              state,
              cursor + (if reg.fold(identity, state) > 0L then value.fold(_.toInt, state(_).toInt) else 1)
            )

      runProgram(Map("p" -> id).withDefaultValue(0))

  def solve2(input: String): Int =
    val instructions = input.linesIterator.map(parseLine).toVector

    val agent0Mailbox = LinkedBlockingQueue[Long]()
    val agent1Mailbox = LinkedBlockingQueue[Long]()
    val agent0        = Agent(instructions, 0, agent1Mailbox, agent0Mailbox)
    val agent1        = Agent(instructions, 1, agent0Mailbox, agent1Mailbox)

    agent0.start()
    agent1.start()

    agent0.join()
    agent1.join()

    agent1.sent.get()

  val input = """set i 31
                |set a 1
                |mul p 17
                |jgz p p
                |mul a 2
                |add i -1
                |jgz i -2
                |add a -1
                |set i 127
                |set p 316
                |mul p 8505
                |mod p a
                |mul p 129749
                |add p 12345
                |mod p a
                |set b p
                |mod b 10000
                |snd b
                |add i -1
                |jgz i -9
                |jgz a 3
                |rcv b
                |jgz b -1
                |set f 0
                |set i 126
                |rcv a
                |rcv b
                |set p a
                |mul p -1
                |add p b
                |jgz p 4
                |snd a
                |set a b
                |jgz 1 3
                |snd b
                |set f 1
                |add i -1
                |jgz i -11
                |snd a
                |jgz f -16
                |jgz a -19""".stripMargin
