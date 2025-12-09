package day6

import scala.io.Source

def parse(inputLines: Iterator[String]): Seq[(Seq[Long], String)] =
  val x = inputLines.map(_.split(" ").filter(_.nonEmpty).toSet).toSeq
  val nums = x.dropRight(1).map(_.map(_.toLong))
  val ops = x.takeRight(1).head
  nums.transpose.zip(ops)

def program(inputLines: Iterator[String]): Long =
  parse(inputLines)
    .map:
      case (xs, op) if op == "*" => xs.product
      case (xs, op) if op == "+" => xs.sum
    .sum

val exampleData =
  """123 328  51 64 
    | 45 64  387 23 
    |  6 98  215 314
    |*   +   *   +  
    |""".stripMargin.linesIterator

lazy val fullData = Source.fromFile("../inputs/day6_input.txt").getLines()

@main def part1_example() =
  val res = program(exampleData)
  println(s"Result: $res")
  assert(res == 4277556)

@main def part1() =
  val res = program(fullData)
  println(s"Result: $res")

@main def part2_example() =
  val res = ???
  println(s"Result: $res")
  assert(res == 3263827)

@main def part2() =
  val res = ???
  println(s"Result: $res")
