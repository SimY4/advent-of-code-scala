package aoc
package y2017

object Day20 extends Input(2017, 20):
  private case class Vector(x: Double, y: Double, z: Double):
    def manhattanNorm: Double = x.abs + y.abs + z.abs

  private case class Particle(p: Vector, v: Vector, a: Vector)

  private def parseLine(line: String): Particle =
    "-?\\d+".r.findAllIn(line).map(_.toDouble).toList match
      case px :: py :: pz :: vx :: vy :: vz :: ax :: ay :: az :: Nil =>
        Particle(Vector(px, py, pz), Vector(vx, vy, vz), Vector(ax, ay, az))

  def solve(input: String): Int =
    input.linesIterator
      .map(parseLine)
      .zipWithIndex
      .minBy((particle, _) => (particle.a.manhattanNorm, particle.v.manhattanNorm, particle.p.manhattanNorm))
      ._2

  private def collide(p1: Particle, p2: Particle): Option[Int] =
    def t(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double): List[Double] =
      val denom = 2.0 * (c - f)
      if denom == 0.0 then ((d - a) / (b - e)) :: Nil
      else
        val root = math.pow(2.0 * b + c - 2.0 * e - f, 2.0) - 8.0 * (a - d) * (c - f)
        if root < 0 then Double.PositiveInfinity :: Nil
        else
          def ff(sign: Double): Double = (-2.0 * b - c + 2.0 * e + f + sign * math.sqrt(root)) / denom
          List(ff(-1.0), ff(1.0)).filter(_ >= 0.0)

    val ts = t(p1.p.x, p1.v.x, p1.a.x, p2.p.x, p2.v.x, p2.a.x) :::
      t(p1.p.y, p1.v.y, p1.a.y, p2.p.y, p2.v.y, p2.a.y) :::
      t(p1.p.z, p1.v.z, p1.a.z, p2.p.z, p2.v.z, p2.a.z)

    val nans = ts.count(_.isNaN)
    val tsInt = ts.flatMap { t =>
      val round = math.round(t * 10000.0).toInt
      if round % 10000 == 0 then Some(round) else None
    }

    tsInt
      .groupBy(identity)
      .toList
      .collectFirst:
        case (t, ts) if ts.size == 3 - nans => t

  def solve2(input: String): Int =
    val particles = input.linesIterator.map(parseLine).toVector
    val destroyed =
      (for
        i   <- particles.indices
        j   <- (i + 1) until particles.size
        t   <- collide(particles(i), particles(j)).toList
        res <- List(t -> i, t -> j)
      yield res)
        .groupBy((t, _) => t)
        .toList
        .sortBy((t, _) => t)
        .map((_, ps) => ps)
        .foldLeft(Set.empty[Int]) { (destroyed, particules) =>
          val collidedParticules = particules.map((_, i) => i).toSet -- destroyed
          if collidedParticules.size > 1 then destroyed ++ collidedParticules
          else destroyed
        }
        .size

    particles.size - destroyed
