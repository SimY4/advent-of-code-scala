package aoc.y2020

import scala.annotation.tailrec

object Day22 {
  def solve(input: String): Int = {
    val players = input.split(System.lineSeparator * 2)
      .map(_.linesIterator.drop(1).map(_.toInt).toList)
      .toList

    @tailrec def play(player1: List[Int], player2: List[Int]): Int = 
      (player1, player2) match {
        case (Nil, player) => player.reverse.zipWithIndex.foldLeft(0) { 
          case (score, (card, mult)) => score + (mult + 1) * card
        }
        case (player, Nil) => player.reverse.zipWithIndex.foldLeft(0) { 
          case (score, (card, mult)) => score + (mult + 1) * card
        }
        case ((f1 :: rest1), (f2 :: rest2)) => 
          if f1 > f2 then play(rest1 ::: List(f1, f2), rest2)
          else play(rest1, rest2 ::: List(f2, f1))
      }

    play(players.head, players.tail.head)
  }

  def solve2(input: String): Int = {
    val players = input.split(System.lineSeparator * 2)
      .map(_.linesIterator.drop(1).map(_.toInt).toList)
      .toList

    def recursiveCombat(player1: List[Int], player2: List[Int]): Boolean = play(player1, player2)._1

    def play(player1: List[Int], player2: List[Int], history: Set[(String, String)] = Set.empty): (Boolean, Int) = 
      if history.contains((player1.mkString, player2.mkString)) then
        true -> player1.reverse.zipWithIndex.foldLeft(0) { 
          case (score, (card, mult)) => score + (mult + 1) * card
        }
      else
        (player1, player2) match {
          case (Nil, player) => false -> player.reverse.zipWithIndex.foldLeft(0) { 
            case (score, (card, mult)) => score + mult * card
          }
          case (player, Nil) => true -> player.reverse.zipWithIndex.foldLeft(0) { 
            case (score, (card, mult)) => score + (mult + 1) * card
          }
          case ((f1 :: rest1), (f2 :: rest2)) if rest1.size >= f1 && rest2.size >= f2 => 
            if recursiveCombat(rest1.take(f1), rest2.take(f2)) then play(rest1 ::: List(f1, f2), rest2, history + (player1.mkString -> player2.mkString))
            else play(rest1, rest2 ::: List(f2, f1), history + (player1.mkString -> player2.mkString))
          case ((f1 :: rest1), (f2 :: rest2)) => 
            if f1 > f2 then play(rest1 ::: List(f1, f2), rest2, history + (player1.mkString -> player2.mkString))
            else play(rest1, rest2 ::: List(f2, f1), history + (player1.mkString -> player2.mkString))
        }

    play(players.head, players.tail.head)._2
  }

  val input = """Player 1:
                |29
                |21
                |38
                |30h
                |25
                |7
                |2
                |36
                |16
                |44
                |20
                |12
                |45
                |4
                |31
                |34
                |33
                |42
                |50
                |14
                |39
                |37
                |11
                |43
                |18
                |
                |Player 2:
                |32
                |24
                |10
                |41
                |13
                |3
                |6
                |5
                |9
                |8
                |48
                |49
                |46
                |17
                |22
                |35
                |1
                |19
                |23
                |28
                |40
                |26
                |47
                |15
                |27""".stripMargin
}