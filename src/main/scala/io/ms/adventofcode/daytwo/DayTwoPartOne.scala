package io.ms.adventofcode.daytwo

import scala.io.Source.fromFile
import scala.util.Using

object DayTwoPartOne {

  enum Move:
    case Rock
    case Paper
    case Scissor

  enum Player:
    case A
    case B

  case class Round(playerA: Move, playerB: Move)

  /*
    let's use bit manipulation to encode rock paper scissor rules

    player A is 1
    player B is 0
    rock is 00
    paper is 01
    scissor is 10

    player A plays rock while player B plays scissors => winner is player A
    player A in bits: 100
    player B in bits: 010
    pattern matching using subtraction => 100 (4 in decimal) - 010 (2 in decimal) = 010 (2 in decimal) that is not multiple of 3 so player A wins
  */
  val rockPaperScissorBitMap: Map[(Player, Move), Int] = Map[(Player, Move), Int](
    (Player.A, Move.Rock) -> Integer.parseInt("100", 2),
    (Player.A, Move.Paper) -> Integer.parseInt("101", 2),
    (Player.A, Move.Scissor) -> Integer.parseInt("110", 2),
    (Player.B, Move.Rock) -> Integer.parseInt("000", 2),
    (Player.B, Move.Paper) -> Integer.parseInt("001", 2),
    (Player.B, Move.Scissor) -> Integer.parseInt("010", 2),
  )

  /**
   * According to the bits pattern, if the difference is a multiple of 3 then player two wins or if the difference is 4 then the game is a draw.
   * In the rest of the cases, player one wins the game.
   * @return 6 if round is win by me, 3 if it's a draw, 0 if I lose
   */
  def getRoundWinner(round: Round): Option[Player] = {
    val bitsDifference = rockPaperScissorBitMap((Player.A, round.playerA)) - rockPaperScissorBitMap((Player.B, round.playerB))
    println(s"bitsDifference: $bitsDifference")
    bitsDifference match {
      case 4 => None
      case _ => if (bitsDifference % 3 == 0) Some(Player.B) else Some(Player.A)
    }
  }

  def computeRoundOutcomeScore(roundOutcome: Option[Player]): Integer = {
    roundOutcome match {
      case Some(Player.A) => 0
      case Some(Player.B) => 6
      case None => 3
    }
  }

  def computeShapeSelectedScore(move: Move): Integer = {
    move match {
      case Move.Rock => 1
      case Move.Paper => 2
      case Move.Scissor => 3
    }
  }

  def getRoundScore(round: Round): Integer = {
    val roundWinner = getRoundWinner(round)
    println(s"round winner: $roundWinner")
    val outcomeScore = computeRoundOutcomeScore(roundWinner)
    println(s"round outcome score: $outcomeScore")
    val shapeSelectedScore = computeShapeSelectedScore(round.playerB)
    println(s"round shape selected score: $shapeSelectedScore")
    val totalScore = outcomeScore + shapeSelectedScore
    println(s"total round score: $totalScore")
    totalScore
  }

  def parseMove(moveCode: String): Move = {
    moveCode match {
      case "A" => Move.Rock
      case "B" => Move.Paper
      case "C" => Move.Scissor
      case "X" => Move.Rock
      case "Y" => Move.Paper
      case "Z" => Move.Scissor
    }
  }

  def parseRound(line: String): Round = {
    val splittedLine = line.split(" ")
    if (splittedLine.length != 2) {
      throw new IllegalArgumentException("Invalid round parsed")
    }
    Round(parseMove(splittedLine(0)), parseMove(splittedLine(1)))
  }

  def computeMyScore(lines: Iterator[String]): Int = {
    lines.foldLeft(0)((myTotalScore: Int, line: String) => {
      println(line)
      val round = parseRound(line)
      myTotalScore + getRoundScore(round)
    })
  }

  def main(args: Array[String]): Unit = {

    Using(fromFile("src/main/resources/daytwo.data")) { file => {
      val myTotalScore = computeMyScore(file.getLines())

      println(s"My total score: $myTotalScore")
    }}
    
  }

}
