package io.ms.adventofcode.dayone

import java.io.{FileNotFoundException, IOException}
import scala.io.Source.*
import scala.util.Try
import scala.util.Using

object DayOnePartOne {

  case class WealthiestElf(id: Int, totalCalories: Int)
  case class CurrentElf(id: Int, var totalCalories: Int)
  case class WealthiestElfSearchState(var wealthiestElf: WealthiestElf, var currentElf: CurrentElf)

  def updateWealthiestElfIfNoMoreWealthiest(wealthiestElf: WealthiestElf, currentElf: CurrentElf): WealthiestElf = {
    if (currentElf.totalCalories > wealthiestElf.totalCalories) {
      return WealthiestElf(currentElf.id, currentElf.totalCalories)
    }

    wealthiestElf
  }

  def updateCurrentElf(currentElf: CurrentElf): CurrentElf = {
    CurrentElf(currentElf.id + 1, 0)
  }

  def incrementCurrentElfCalories(currentElf: CurrentElf, calories: Int): CurrentElf = {
    currentElf.totalCalories += calories
    currentElf
  }

  def searchWealthiestElf(lines: Iterator[String]): WealthiestElf = {
    val partialSearchResult = lines.foldLeft(WealthiestElfSearchState(WealthiestElf(1, 0), CurrentElf(1, 0)))((searchState, line) => {
      line.toIntOption match {
        case Some(value) => {
          searchState.currentElf = incrementCurrentElfCalories(searchState.currentElf, line.toIntOption.getOrElse(0))
          searchState
        }
        case default => {
          val wealthiestElf = updateWealthiestElfIfNoMoreWealthiest(searchState.wealthiestElf, searchState.currentElf)
          val currentElf = updateCurrentElf(searchState.currentElf)
          WealthiestElfSearchState(wealthiestElf, currentElf)
        }
      }
    })
    updateWealthiestElfIfNoMoreWealthiest(partialSearchResult.wealthiestElf, partialSearchResult.currentElf)
  }

  def main(args: Array[String]): Unit = {

    Using(fromFile("src/main/resources/dayone.data")) { file => {
      val wealthiestElf = searchWealthiestElf(file.getLines())
      println(s"Wealthiest Elf ID: ${wealthiestElf.id}")
      println(s"Wealthiest Elf Total Calories: ${wealthiestElf.totalCalories}")
    }}

  }

}
