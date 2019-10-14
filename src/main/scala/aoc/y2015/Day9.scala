package aoc.y2015

object Day9
  import scala.collection.parallel.CollectionConverters._

  case class Route(from: String, to: String, distance: Int)

  private val linePattern = "(\\w+) to (\\w+) = (\\d+)".r

  private def paths(input: String): Iterable[List[Route]] =
    def expandPath(path: List[Route], rest: Map[String, Seq[Route]]): Iterable[List[Route]] =
      (for
        nextRoutes <- rest.get(path.head.to)
        filtered = nextRoutes.filter { nextRoute => path.forall(_.from != nextRoute.to) }
        if filtered.nonEmpty
      yield filtered) match
        case None => path :: Nil
        case Some(nextRoutes) =>
          (for
            nextRoute <- nextRoutes.par
            nextPath <- expandPath(nextRoute :: path, rest - path.head.to)
          yield nextPath).seq

    val routes = input.linesIterator.toSeq
      .flatMap { case linePattern(from, to, distance) => 
        Seq(Route(from, to, distance.toInt), Route(to, from, distance.toInt)) 
      }
      .groupBy(_.from)
      
    (for
      initialRoute <- routes.values.flatten.par
      path <- expandPath(initialRoute :: Nil, routes - initialRoute.from)
      if path.size >= ((routes.size / 2) - 1)
    yield path).seq

  def solve(input: String): Int =
   paths(input)
      .map { path => path.map(_.distance).sum }
      .min

  def solve2(input: String): Int =
    paths(input)
      .map { path => path.map(_.distance).sum }
      .max

  val input = """AlphaCentauri to Snowdin = 66
                |AlphaCentauri to Tambi = 28
                |AlphaCentauri to Faerun = 60
                |AlphaCentauri to Norrath = 34
                |AlphaCentauri to Straylight = 34
                |AlphaCentauri to Tristram = 3
                |AlphaCentauri to Arbre = 108
                |Snowdin to Tambi = 22
                |Snowdin to Faerun = 12
                |Snowdin to Norrath = 91
                |Snowdin to Straylight = 121
                |Snowdin to Tristram = 111
                |Snowdin to Arbre = 71
                |Tambi to Faerun = 39
                |Tambi to Norrath = 113
                |Tambi to Straylight = 130
                |Tambi to Tristram = 35
                |Tambi to Arbre = 40
                |Faerun to Norrath = 63
                |Faerun to Straylight = 21
                |Faerun to Tristram = 57
                |Faerun to Arbre = 83
                |Norrath to Straylight = 9
                |Norrath to Tristram = 50
                |Norrath to Arbre = 60
                |Straylight to Tristram = 27
                |Straylight to Arbre = 81
                |Tristram to Arbre = 90""".stripMargin
