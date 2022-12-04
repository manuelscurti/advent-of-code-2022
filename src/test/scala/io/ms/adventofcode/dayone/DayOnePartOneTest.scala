package io.ms.adventofcode.dayone
import io.ms.adventofcode.dayone.DayOnePartOne.{CurrentElf, WealthiestElf}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*

class DayOnePartOneTest extends AnyFlatSpec with should.Matchers {

  "updateWealthiestElfIfNoMoreWealthiest" should "update wealthiest elf when given elf overcomes the amount of calories of the wealthiest elf" in {
    DayOnePartOne.updateWealthiestElfIfNoMoreWealthiest(WealthiestElf(1, 10), CurrentElf(2, 20)) should be (WealthiestElf(2, 20))
  }

  "updateCurrentElf" should "return a elf with id equals to the latest elf id visited + 1" in {
    DayOnePartOne.updateCurrentElf(CurrentElf(2, 20)) should be (CurrentElf(3, 0))
  }

  "incrementCurrentElfCalories" should "add given calories to the current elf's total calories" in {
    DayOnePartOne.incrementCurrentElfCalories(CurrentElf(1, 20), 30) should be (CurrentElf(1, 50))
  }

  "searchWealthiestElf" should "return elf with the max amount of calories in the bag" in {
    val input = """
    1000
    2000
    3000

    4000

    5000
    6000

    7000
    8000
    9000

    10000
    """.stripMargin

    DayOnePartOne.searchWealthiestElf(Seq("1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000").iterator) should be (WealthiestElf(4, 24000))
  }
}
