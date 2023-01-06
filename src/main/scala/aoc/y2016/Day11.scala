package aoc.y2016

import scala.annotation.tailrec
import scala.collection.mutable.{ HashSet, Queue }

object Day11:
  enum Facility(val name: String):
    case Microchip(override val name: String) extends Facility(name)
    case Generator(override val name: String) extends Facility(name)

  import Facility.*

  private def isValid(f: Seq[Facility]): Boolean =
    f.forall {
      case Microchip(name) => f.forall(_.isInstanceOf[Microchip]) || f.contains(Generator(name))
      case Generator(name) => true
    }

  private case class State(elevator: Int, items: Map[Facility, Int])

  def solve(input: List[Set[Facility]]): Int =
    val seen        = HashSet.empty[State]
    val startState  = input.zipWithIndex.flatMap((floor, i) => floor.map(_ -> i)).toMap
    val statesQueue = Queue(State(0, startState) -> 0)

    def itemsOnFloor(floor: Int, state: State): Seq[Facility] =
      state.items.toSeq.collect { case (item, `floor`) => item }

    def canGoInElevator(a: Facility, b: Facility): Boolean =
      (a, b) match
        case (Microchip(m), Generator(g)) => m == g
        case (Generator(g), Microchip(m)) => g == m
        case _                            => true

    def nextStates(elevator: Int, items: Map[Facility, Int]): Seq[State] =
      val onThisFloor = itemsOnFloor(elevator, State(elevator, items))
      val moves = for
        i <- onThisFloor.indices
        j <- i until onThisFloor.size
        a = onThisFloor(i)
        b = onThisFloor(j)
        if canGoInElevator(a, b)
      yield Set(a, b)

      def getStates(newFloor: Int, moves: Seq[Set[Facility]]): Seq[State] =
        for
          move <- moves
          newState = State(newFloor, move.foldLeft(items)((acc, item) => acc.updated(item, newFloor)))
          if List(elevator, newFloor).forall(floor => isValid(itemsOnFloor(floor, newState)))
        yield newState

      elevator match
        case 3     => getStates(2, moves)
        case 0     => getStates(1, moves)
        case floor => getStates(floor - 1, moves) ++ getStates(floor + 1, moves)
    end nextStates

    @tailrec def go(): Int =
      val (State(el, items), n) = statesQueue.dequeue()
      if items.forall((_, floor) => floor == 3) then n
      else
        for
          nextState <- nextStates(el, items)
          if seen.add(nextState)
        do statesQueue.enqueue(nextState -> (n + 1))
        go()

    go()

  def solve2(input: List[Set[Facility]]): Int = solve(input)

  val input1 = List(
    Set(
      Generator("polonium"),
      Generator("thulium"),
      Microchip("thulium"),
      Generator("promethium"),
      Generator("ruthenium"),
      Microchip("ruthenium"),
      Generator("cobalt"),
      Microchip("cobalt")
    ),
    Set(Microchip("polonium"), Microchip("promethium")),
    Set.empty[Facility],
    Set.empty[Facility]
  )

  val input2 = input1.updated(
    0,
    input1(0) ++ Set(Microchip("elerium"), Generator("elerium"), Microchip("dilithium"), Generator("dilithium"))
  )
