package day7

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

def compute(manifold: List[Array[Char]]): Int =
  val start = manifold.head.indexWhere(_ == 'S')
  manifold.tail
    .foldLeft(Set(start), 0):
      case ((beams, splits), row) =>
        row.zipWithIndex
          .filter: (r, idx) =>
            beams(idx) && r == '^'
          .map(_._2)
          .pipe: bsplit =>
            val updatedBeams =
              beams ++ bsplit.flatMap(i => List(i - 1, i + 1)) -- bsplit
            updatedBeams -> (splits + bsplit.length)
    ._2

def program(inputLines: Iterator[String]): Long =
  val input = inputLines.map(_.toCharArray).toList
  compute(input)

def program2(inputLines: Iterator[String]): Long =
  ???

val exampleData =
  """.......S.......
    |...............
    |.......^.......
    |...............
    |......^.^......
    |...............
    |.....^.^.^.....
    |...............
    |....^.^...^....
    |...............
    |...^.^...^.^...
    |...............
    |..^...^.....^..
    |...............
    |.^.^.^.^.^...^.
    |...............
    |""".stripMargin.linesIterator

lazy val fullData = Source.fromFile("../inputs/day7_input.txt").getLines()

@main def part1_example() =
  val res = program(exampleData)
  println(s"Result: $res")
  assert(res == 21)

@main def part1() =
  val res = program(fullData)
  println(s"Result: $res")

@main def part2_example() =
  val res = program2(exampleData)
  println(s"Result: $res")
  assert(res == 40)

@main def part2() =
  val res = program2(fullData)
  println(s"Result: $res")
