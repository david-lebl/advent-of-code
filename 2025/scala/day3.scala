package day3

import scala.io.Source
import scala.util.chaining.*

type Bank = Seq[Short]
object Bank:
  def fromString(s: String): Bank = s.map(_.toString.toShort)

def findLargest(bank: Bank) =
  val a = bank.dropRight(1).max
  val b = bank.dropWhile(_ != a).drop(1).max
  s"$a$b".toShort

def program(banks: Iterator[String]): Long =
  banks
    .map(Bank.fromString)
    .map(findLargest)
    .sum

val exampleData =
  """987654321111111
    |811111111111119
    |234234234234278
    |818181911112111
    |""".stripMargin.linesIterator

val fullData = Source.fromFile("../inputs/day3_input.txt").getLines()

@main def part1_example() =
  val res = program(exampleData)
  println(s"Result: $res")
  assert(res == 357)

@main def part1() =
  val res = program(fullData)
  println(s"Result: $res")

@main def part2_example() =
  ???
  val res = program(exampleData)
  println(s"Result: $res")
  assert(res == 3121910778619L)

@main def part2() =
  ???
  val res = program(fullData)
  println(s"Result: $res")
