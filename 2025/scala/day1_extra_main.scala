package day1_extra

import scala.io.Source

@main def part1_example(): Unit =
  val res = program_part1(exampleData)
  println(s"Result part-1 = $res")
  assert(res == 3)

@main def part1(): Unit =
  val res = program_part1(fullData)
  println(s"Result part-1 = $res")

@main def part2_example(): Unit =
  val res = program_part2(exampleData)
  println(s"Result part-2 = $res")
  assert(res == 6)

@main def part2(): Unit =
  val res = program_part2(fullData)
  println(s"Result part-2 = $res")


val exampleData =
  """L68
    |L30
    |R48
    |L5
    |R60
    |L55
    |L1
    |L99
    |R14
    |L82
    |""".stripMargin.linesIterator

def fullData = Source.fromFile("../inputs/day1_input.txt").getLines()
