package io.ms.adventofcode.dayone

import scala.collection.mutable
import scala.io.Source._



object DayOnePartTwo {

  case class WealthiestElvesTopThree(elves: mutable.PriorityQueue[Elf])
  case class Elf(id: Int, var totalCalories: Int)
  case class WealthiestElfSearchState(wealthiestElves: mutable.PriorityQueue[Elf], var currentElf: Elf)

  def updateWealthiestElvesTop3(wealthiestElves: mutable.PriorityQueue[Elf], currentElf: Elf): mutable.PriorityQueue[Elf]  = {
    wealthiestElves.addOne(currentElf)
    if (wealthiestElves.size < 3) {
      return wealthiestElves
    }

    val pq = buildElvesRanking(wealthiestElves.dequeue(), wealthiestElves.dequeue(), wealthiestElves.dequeue())
    pq
  }

  def updateCurrentElf(currentElf: Elf): Elf = {
    Elf(currentElf.id + 1, 0)
  }

  def incrementCurrentElfCalories(currentElf: Elf, calories: Int): Elf = {
    currentElf.totalCalories += calories
    currentElf
  }

  def buildElvesRanking(elves: Elf*): mutable.PriorityQueue[Elf] = {
    val pq = mutable.PriorityQueue()(Ordering.by[Elf, Int](_.totalCalories))
    pq.addAll(elves)
    pq
  }

  def searchWealthiestElf(lines: Iterator[String]): List[Elf] = {
    val partialSearchResult = lines.foldLeft(WealthiestElfSearchState(buildElvesRanking(), Elf(1, 0)))((searchState, line) => {
      println(line)
      line.toIntOption match {
        case Some(value) => {
          searchState.currentElf = incrementCurrentElfCalories(searchState.currentElf, line.toIntOption.getOrElse(0))
          searchState
        }
        case default => {
          val wealthiestElf = updateWealthiestElvesTop3(searchState.wealthiestElves, searchState.currentElf)
          val currentElf = updateCurrentElf(searchState.currentElf)
          WealthiestElfSearchState(wealthiestElf, currentElf)
        }
      }
    })
    updateWealthiestElvesTop3(partialSearchResult.wealthiestElves, partialSearchResult.currentElf).toList
  }

  def main(args: Array[String]): Unit = {

    val file = fromFile("src/main/resources/dayone.data")

    val wealthiestElves = searchWealthiestElf(file.getLines())

    file.close()

    println(s"Total elves in the ranking: ${wealthiestElves.size}")
    println(s"Wealthiest Elves Top 3: ${wealthiestElves}")
    println(s"Wealthiest Elves Top 3 total calories: ${wealthiestElves.foldLeft(0)(_ + _.totalCalories)}")
  }

}
