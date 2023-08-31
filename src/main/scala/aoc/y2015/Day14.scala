package aoc.y2015

object Day14:
  private val linePattern =
    "^([A-Z][a-z]+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds\\.$".r

  final private case class Raindeer(name: String, speed: Int, flyTime: Int, restTime: Int)
  final private case class State(distance: Int, score: Int, switch: Int, status: Status)

  private enum Status extends Enum[Status]:
    case Flying, Resting

  def solve(input: String): Int =
    val raindeers = (for case linePattern(raindeer, speed, flyTime, restTime) <- input.linesIterator
    yield Raindeer(raindeer, speed.toInt, flyTime.toInt, restTime.toInt) -> State(
      0,
      0,
      flyTime.toInt,
      Status.Flying
    )).toMap

    LazyList
      .from(1)
      .scanLeft(raindeers): (states, second) =>
        states.transform:
          case (Raindeer(_, speed, _, restTime), State(distance, score, switch, Status.Flying)) =>
            if second < switch then State(distance + speed, score, switch, Status.Flying)
            else State(distance + speed, score, second + restTime, Status.Resting)
          case (Raindeer(_, _, flyTime, _), s @ State(distance, score, switch, Status.Resting)) =>
            if second < switch then s
            else State(distance, score, second + flyTime, Status.Flying)
      .drop(2502)
      .head
      .values
      .map(_.distance)
      .max

  def solve2(input: String): Int =
    val raindeers = (for case linePattern(raindeer, speed, flyTime, restTime) <- input.linesIterator
    yield Raindeer(raindeer, speed.toInt, flyTime.toInt, restTime.toInt) -> State(
      0,
      0,
      flyTime.toInt,
      Status.Flying
    )).toMap

    LazyList
      .from(1)
      .scanLeft(raindeers): (states, second) =>
        val updatedStates = states.transform:
          case (Raindeer(_, speed, _, restTime), State(distance, score, switch, Status.Flying)) =>
            if second < switch then State(distance + speed, score, switch, Status.Flying)
            else State(distance + speed, score, second + restTime, Status.Resting)
          case (Raindeer(_, _, flyTime, _), s @ State(distance, score, switch, Status.Resting)) =>
            if second < switch then s
            else State(distance, score, second + flyTime, Status.Flying)
        val leaders = updatedStates.toList
          .groupMap(_._2.distance)(_._1)
          .maxBy(_._1)
          ._2
        leaders
          .foldLeft(updatedStates): (s, leadingRaindeer) =>
            val state = s(leadingRaindeer)
            s.updated(leadingRaindeer, state.copy(score = state.score + 1))
      .drop(2502)
      .head
      .values
      .map(_.score)
      .max

  val input = """Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
                |Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.
                |Rudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.
                |Cupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.
                |Donner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.
                |Dasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.
                |Comet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.
                |Prancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.
                |Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds.""".stripMargin
