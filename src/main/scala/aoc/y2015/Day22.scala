package aoc.y2015

import java.util.concurrent.atomic.AtomicInteger

object Day22:
  private sealed trait EffectOverTime(val turns: Int)
  private enum Spell(val mana: Int) extends Enum[Spell]:
    case MagicMisile extends Spell(53)
    case Drain       extends Spell(73)
    case Shield      extends Spell(113) with EffectOverTime(6)
    case Poison      extends Spell(173) with EffectOverTime(6)
    case Recharge    extends Spell(229) with EffectOverTime(5)

  private object Spell:
    val book: List[Spell] = List(MagicMisile, Drain, Shield, Poison, Recharge)

  import Spell.*

  final private case class Character(hitPoints: Int, mana: Int)
  final private case class Boss(hitPoints: Int, damage: Int)
  final private case class GameState(cooldowns: Map[EffectOverTime, Int], character: Character, boss: Boss)

  def solve(input: String): Int =
    val boss = input.linesIterator.toList match
      case s"Hit Points: $hp" :: s"Damage: $damage" :: Nil => Boss(hp.toInt, damage.toInt)

    val atom = new AtomicInteger(Int.MaxValue)

    def duel(myTurn: Boolean, spent: Int, state: GameState): List[Int] =
      // turn start effects
      val turnStartState = state.cooldowns.keys
        .foldLeft(state):
          case (state, Poison)   => state.copy(boss = state.boss.copy(hitPoints = state.boss.hitPoints - 3))
          case (state, Recharge) => state.copy(character = state.character.copy(mana = state.character.mana + 101))
          case (state, _)        => state
        .copy(cooldowns = state.cooldowns.view.mapValues(_ - 1).filter((_, v) => v > 0).toMap)
      val armor = state.cooldowns.keys
        .collect:
          case Shield => 7
        .sum

      if turnStartState.boss.hitPoints <= 0 then
        atom.getAndUpdate(min => if spent < min then spent else min)
        spent :: Nil
      else if myTurn then
        for
          spell <- Spell.book
          if spell.mana <= turnStartState.character.mana && !turnStartState.cooldowns.keys.exists(_ == spell)
          endTurnState = spell match
            case MagicMisile =>
              turnStartState.copy(
                character = turnStartState.character.copy(mana = turnStartState.character.mana - MagicMisile.mana),
                boss = turnStartState.boss.copy(hitPoints = turnStartState.boss.hitPoints - 4)
              )
            case Drain =>
              turnStartState.copy(
                character = turnStartState.character.copy(
                  hitPoints = turnStartState.character.hitPoints + 2,
                  mana = turnStartState.character.mana - Drain.mana
                ),
                boss = turnStartState.boss.copy(hitPoints = turnStartState.boss.hitPoints - 2)
              )
            case eot: EffectOverTime =>
              turnStartState.copy(
                cooldowns = turnStartState.cooldowns + (eot -> eot.turns),
                character = turnStartState.character.copy(mana = turnStartState.character.mana - eot.mana)
              )
          outcome <-
            if endTurnState.boss.hitPoints <= 0 then
              atom.getAndUpdate(min => if spell.mana + spent < min then spell.mana + spent else min)
              (spell.mana + spent) :: Nil
            else if spell.mana + spent < atom.get then duel(false, spell.mana + spent, endTurnState)
            else Nil
        yield outcome
      else
        val endTurnState = turnStartState.copy(character =
          turnStartState.character.copy(hitPoints =
            turnStartState.character.hitPoints - math.max(turnStartState.boss.damage - armor, 1)
          )
        )
        if endTurnState.character.hitPoints <= 0 then Nil
        else duel(true, spent, endTurnState)

    duel(true, 0, GameState(Map.empty, Character(50, 500), boss)).min

  def solve2(input: String): Int =
    val boss = input.linesIterator.toList match
      case s"Hit Points: $hp" :: s"Damage: $damage" :: Nil => Boss(hp.toInt, damage.toInt)

    val atom = new AtomicInteger(Int.MaxValue)

    def duel(myTurn: Boolean, spent: Int, state: GameState): List[Int] =
      val initialState =
        if myTurn then state.copy(character = state.character.copy(hitPoints = state.character.hitPoints - 1))
        else state
      if initialState.character.hitPoints <= 0 then Nil
      else
        // turn start effects
        val turnStartState = initialState.cooldowns.keys
          .foldLeft(initialState):
            case (state, Poison)   => state.copy(boss = state.boss.copy(hitPoints = state.boss.hitPoints - 3))
            case (state, Recharge) => state.copy(character = state.character.copy(mana = state.character.mana + 101))
            case (state, _)        => state
          .copy(cooldowns = state.cooldowns.view.mapValues(_ - 1).filter((_, v) => v > 0).toMap)
        val armor = initialState.cooldowns.keys
          .collect:
            case Shield => 7
          .sum

        if turnStartState.boss.hitPoints <= 0 then
          atom.getAndUpdate(min => if spent < min then spent else min)
          spent :: Nil
        else if myTurn then
          for
            spell <- Spell.book
            if spell.mana <= turnStartState.character.mana && !turnStartState.cooldowns.keys.exists(_ == spell)
            endTurnState = spell match
              case MagicMisile =>
                turnStartState.copy(
                  character = turnStartState.character.copy(
                    hitPoints = turnStartState.character.hitPoints,
                    mana = turnStartState.character.mana - MagicMisile.mana
                  ),
                  boss = turnStartState.boss.copy(hitPoints = turnStartState.boss.hitPoints - 4)
                )
              case Drain =>
                turnStartState.copy(
                  character = turnStartState.character.copy(
                    hitPoints = turnStartState.character.hitPoints + 2,
                    mana = turnStartState.character.mana - Drain.mana
                  ),
                  boss = turnStartState.boss.copy(hitPoints = turnStartState.boss.hitPoints - 2)
                )
              case eot: EffectOverTime =>
                turnStartState.copy(
                  cooldowns = turnStartState.cooldowns + (eot -> eot.turns),
                  character = turnStartState.character.copy(
                    hitPoints = turnStartState.character.hitPoints,
                    mana = turnStartState.character.mana - eot.mana
                  )
                )
            outcome <-
              if endTurnState.boss.hitPoints <= 0 then
                atom.getAndUpdate(min => if spell.mana + spent < min then spent + spent else min)
                (spell.mana + spent) :: Nil
              else if spell.mana + spent < atom.get then duel(false, spell.mana + spent, endTurnState)
              else Nil
          yield outcome
        else
          val endTurnState = turnStartState.copy(character =
            turnStartState.character.copy(hitPoints =
              turnStartState.character.hitPoints - math.max(turnStartState.boss.damage - armor, 1)
            )
          )
          if endTurnState.character.hitPoints <= 0 then Nil
          else duel(true, spent, endTurnState)

    duel(true, 0, GameState(Map.empty, Character(50, 500), boss)).min

  val input = """Hit Points: 51
                |Damage: 9""".stripMargin
