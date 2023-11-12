package aoc.y2022

import scala.collection.immutable.ListMap

object Day11:
  final private case class Test(divisibleBy: Long, ifTrue: String, ifFalse: String):
    def apply(item: Long): String = if item % divisibleBy == 0 then ifTrue else ifFalse

  final private case class Monkey(id: String, items: List[Long], ops: (Long) => Long, test: Test):
    override def toString = items.mkString(", ")

  private def parse(input: String): Monkey =
    input.linesIterator.toList match
      case s"Monkey $id:" ::
          s"  Starting items: $items" ::
          s"  Operation: new = $opX $op $opY" ::
          s"  Test: divisible by $divisibleBy" ::
          s"    If true: throw to monkey $ifTrue" ::
          s"    If false: throw to monkey $ifFalse" :: Nil =>
        val parsedOpX = opX.toLongOption
        val parsedOpY = opY.toLongOption
        val operation = op match
          case "*" => (x: Long) => parsedOpX.getOrElse(x) * parsedOpY.getOrElse(x)
          case "+" => (x: Long) => parsedOpX.getOrElse(x) + parsedOpY.getOrElse(x)
        Monkey(
          id,
          items.split(", ").map(_.toLong).toList,
          operation,
          Test(divisibleBy.toInt, ifTrue, ifFalse)
        )

  def solve(input: String): Int =
    val monkeys = input.split("\n\n").map(parse).map(m => m.id -> m).to(ListMap)

    LazyList
      .from(0)
      .scanLeft(monkeys -> Map.empty[String, Int]):
        case ((monkeys, counter), _) =>
          monkeys.keys.foldLeft(monkeys -> counter):
            case ((acc, counter), id) =>
              val monkey = acc(id)
              (
                monkey.items.foldLeft(acc.updated(id, monkey.copy(items = Nil))): (acc, item) =>
                  val newItem = monkey.ops(item) / 3
                  val throwTo = monkey.test(newItem)
                  acc.updatedWith(throwTo)(_.map(m => m.copy(items = newItem :: m.items)))
                ,
                counter.updatedWith(id):
                  case Some(cnt) => Some(cnt + monkey.items.size)
                  case None      => Some(monkey.items.size)
              )
      .drop(20)
      .map((_, counter) => counter.values.toList.sorted.takeRight(2).product)
      .head

  def solve2(input: String): BigInt =
    val monkeys = input.split("\n\n").map(parse).map(m => m.id -> m).to(ListMap)
    val modulus = monkeys.values.map(_.test.divisibleBy).product

    LazyList
      .from(0)
      .scanLeft(monkeys -> Map.empty[String, Int]):
        case ((monkeys, counter), _) =>
          monkeys.keys.foldLeft(monkeys -> counter):
            case ((acc, counter), id) =>
              val monkey = acc(id)
              (
                monkey.items.foldLeft(acc.updated(id, monkey.copy(items = Nil))): (acc, item) =>
                  val newItem = monkey.ops(item) % modulus
                  val throwTo = monkey.test(newItem)
                  acc.updatedWith(throwTo)(_.map(m => m.copy(items = newItem :: m.items)))
                ,
                counter.updatedWith(id):
                  case Some(cnt) => Some(cnt + monkey.items.size)
                  case None      => Some(monkey.items.size)
              )
      .drop(10000)
      .map((_, counter) => counter.values.toList.sorted.takeRight(2).map(BigInt(_)).product)
      .head

  val input = """Monkey 0:
                |  Starting items: 78, 53, 89, 51, 52, 59, 58, 85
                |  Operation: new = old * 3
                |  Test: divisible by 5
                |    If true: throw to monkey 2
                |    If false: throw to monkey 7
                |
                |Monkey 1:
                |  Starting items: 64
                |  Operation: new = old + 7
                |  Test: divisible by 2
                |    If true: throw to monkey 3
                |    If false: throw to monkey 6
                |
                |Monkey 2:
                |  Starting items: 71, 93, 65, 82
                |  Operation: new = old + 5
                |  Test: divisible by 13
                |    If true: throw to monkey 5
                |    If false: throw to monkey 4
                |
                |Monkey 3:
                |  Starting items: 67, 73, 95, 75, 56, 74
                |  Operation: new = old + 8
                |  Test: divisible by 19
                |    If true: throw to monkey 6
                |    If false: throw to monkey 0
                |
                |Monkey 4:
                |  Starting items: 85, 91, 90
                |  Operation: new = old + 4
                |  Test: divisible by 11
                |    If true: throw to monkey 3
                |    If false: throw to monkey 1
                |
                |Monkey 5:
                |  Starting items: 67, 96, 69, 55, 70, 83, 62
                |  Operation: new = old * 2
                |  Test: divisible by 3
                |    If true: throw to monkey 4
                |    If false: throw to monkey 1
                |
                |Monkey 6:
                |  Starting items: 53, 86, 98, 70, 64
                |  Operation: new = old + 6
                |  Test: divisible by 7
                |    If true: throw to monkey 7
                |    If false: throw to monkey 0
                |
                |Monkey 7:
                |  Starting items: 88, 64
                |  Operation: new = old * old
                |  Test: divisible by 17
                |    If true: throw to monkey 2
                |    If false: throw to monkey 5""".stripMargin
