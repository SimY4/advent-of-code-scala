package aoc.y2015

object Day21 {
  private enum Item(name: String, cost: Int, damage: Int, armor: Int) {
    case Weapon(name: String, cost: Int, damage: Int) extends Item(name, cost, damage, 0)
    case Armor(name: String, cost: Int, armor: Int) extends Item(name, cost, 0, armor)
    case Ring(name: String, cost: Int, damage: Int, armor: Int) extends Item(name, cost, damage, armor)
  }

  private final case class Character(hitPoints: Int, damage: Int, armor: Int)

  import Item._

  private val items = List(
    Weapon("Dagger", 8, 4),
    Weapon("Shortsword", 10, 5),
    Weapon("Warhammer", 25, 6),
    Weapon("Longsword", 40, 7),
    Weapon("Greataxe", 74, 8),
    
    Armor("Leather", 13, 1),
    Armor("Chainmail", 31, 2),
    Armor("Splintmail", 53, 3),
    Armor("Bandedmail", 75, 4),
    Armor("Platemail", 102, 5),
    
    Ring("Damage +1", 25, 1, 0),
    Ring("Damage +2", 50, 2, 0),
    Ring("Damage +3", 100, 3, 0),
    Ring("Defense +1", 20, 0, 1),
    Ring("Defense +2", 40, 0, 2),
    Ring("Defense +3", 80, 0, 3)
  )

  private def duel(p1: Character, p2: Character): Boolean = {
    val p1Damage = math.max(p1.damage - p2.armor, 1)
    val p2Damage = math.max(p2.damage - p1.armor, 1)

    val p1Strikes = 
      if (p2.hitPoints % p1Damage == 0) p2.hitPoints / p1Damage
      else (p2.hitPoints / p1Damage) + 1
    val p2Strikes = 
      if (p1.hitPoints % p2Damage == 0) p1.hitPoints / p2Damage
      else (p1.hitPoints / p2Damage) + 1

    p1Strikes <= p2Strikes
  }

  def solve(input: String): Int = {
    val boss = input.linesIterator
      .flatMap(line => "\\d+".r.findFirstIn(line).map(_.toInt))
      .toList match {
        case hp :: damage :: armor :: Nil => Character(hp, damage, armor)
      }

    val weapons = items.collect { case w: Weapon => w }
    val armor = items.collect { case a: Armor => a }.foldRight(List(Option.empty[Armor])) { Some(_) :: _ }
    val rings = (List.empty[Ring] :: (for {
      r1 <- items.collect { case r: Ring => r }
      r2 <- items.collect { case r: Ring => r }
    } yield if (r1 == r2) r1 :: Nil else r1 :: r2 :: Nil))
      .distinct

    (for {
      weapon <- weapons
      ar <- armor
      rngs <- rings
      char = Character(100, weapon.damage + rngs.map(_.damage).sum, ar.fold(0)(_.armor) + rngs.map(_.armor).sum)
      if duel(char, boss)
    } yield weapon.cost + ar.fold(0)(_.cost) + rngs.map(_.cost).sum)
      .min
  }

  def solve2(input: String): Int = {
    val boss = input.linesIterator
      .flatMap(line => "\\d+".r.findFirstIn(line).map(_.toInt))
      .toList match {
        case hp :: damage :: armor :: Nil => Character(hp, damage, armor)
      }

    val weapons = items.collect { case w: Weapon => w }
    val armor = items.collect { case a: Armor => a }.foldRight(List(Option.empty[Armor])) { Some(_) :: _ }
    val rings = (List.empty[Ring] :: (for {
      r1 <- items.collect { case r: Ring => r }
      r2 <- items.collect { case r: Ring => r }
    } yield if (r1 == r2) r1 :: Nil else r1 :: r2 :: Nil))
      .distinct

    (for {
      weapon <- weapons
      ar <- armor
      rngs <- rings
      char = Character(100, weapon.damage + rngs.map(_.damage).sum, ar.fold(0)(_.armor) + rngs.map(_.armor).sum)
      if !duel(char, boss)
    } yield weapon.cost + ar.fold(0)(_.cost) + rngs.map(_.cost).sum)
      .max
  }

  val input = """Hit Points: 103
                |Damage: 9
                |Armor: 2""".stripMargin
}
