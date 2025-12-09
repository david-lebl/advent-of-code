package day5

import scala.io.Source

def parse(input: String) =
  val lines = input.linesIterator
  val Array(sRanges, sIds) = input.split("\n\n")
  val ranges = sRanges.linesIterator.toList.map:
    case s"$a-$b" => a.toLong to b.toLong
  val ids = sIds.linesIterator.map(_.toLong).toList
  (ranges = ranges, ids = ids)

def program(input: String): Long =
  val x = parse(input)
  x.ids.count: id =>
    x.ranges.exists: r =>
      id >= r.start && id <= r.end

def program2(input: String): Long =
  val x = parse(input)
  x.ranges.flatten.distinct.size

val exampleData =
  """3-5
    |10-14
    |16-20
    |12-18
    |
    |1
    |5
    |8
    |11
    |17
    |32
    |""".stripMargin

lazy val fullData = Source.fromFile("../inputs/day5_input.txt").mkString

@main def part1_example() =
  val res = program(exampleData)
  println(s"Result: $res")
  assert(res == 3)

@main def part1() =
  val res = program(fullData)
  println(s"Result: $res")

@main def part2_example() =
  val res = program2(exampleData)
  println(s"Result: $res")
  assert(res == 14)

@main def part2() =
  val res = program2(fullData)
  println(s"Result: $res")
