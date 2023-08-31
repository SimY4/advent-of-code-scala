package aoc.y2015

object Day15:
  private def split(number: Int, parts: Int): Vector[List[Int]] =
    LazyList
      .iterate(Vector.range(1, number + 1).map(_ :: Nil)): acc =>
        for
          xxs <- acc
          xs  <- 1 until number
          concat = xs :: xxs
          if concat.sum <= number
        yield concat
      .drop(parts - 1)
      .head
      .filter(_.sum == number)

  def solve(input: String): Int =
    val ingredients = (for
      line <- input.linesIterator
      parsed = "-?\\d+".r.findAllIn(line).map(_.toInt).toList
    yield parsed).toList

    split(100, ingredients.size)
      .map: fractions =>
        fractions
          .zip(ingredients)
          .map((fr, stats) => stats.init.map(_ * fr))
          .reduce((is1, is2) => is1.zip(is2).map(_ + _))
          .map(_ max 0)
          .product
      .max

  def solve2(input: String): Int =
    val ingredients = (for
      line <- input.linesIterator
      parsed = "-?\\d+".r.findAllIn(line).map(_.toInt).toList
    yield parsed).toList

    split(100, ingredients.size)
      .flatMap: fractions =>
        val recipe = fractions
          .zip(ingredients)
          .map((fr, stats) => stats.map(_ * fr))
          .reduce((is1, is2) => is1.zip(is2).map(_ + _))
          .map(_ max 0)
        Option.when(recipe.last == 500):
          recipe.init.product
      .max

  val input = """Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
                |Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
                |Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
                |Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1""".stripMargin
