package day4

import scala.io.Source

type Grid = Array[Array[Char]]
extension (grid: Grid)
  def isDefinedAt(x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && y < grid.length && x < grid(y).length

object Grid:
  def fromString(s: String): Grid =
    s.split('\n').map(_.toCharArray)

def program(grid: Grid): Long =
  (for
    y <- grid.indices
    x <- grid(y).indices
    if grid(y)(x) == '@'
    papers =
      for
        ny <- y - 1 to y + 1
        nx <- x - 1 to x + 1
        if grid.isDefinedAt(nx, ny)
        if (x, y) != (nx, ny)
      yield grid(ny)(nx)
    cnt = papers.count(_ == '@')
  yield cnt).count(_ < 4)

def program(matrix: String): Long = program(Grid.fromString(matrix))

val exampleData =
  """..@@.@@@@.
    |@@@.@.@.@@
    |@@@@@.@.@@
    |@.@@@@..@.
    |@@.@@@@.@@
    |.@@@@@@@.@
    |.@.@.@.@@@
    |@.@@@.@@@@
    |.@@@@@@@@.
    |@.@.@@@.@.
    |""".stripMargin

lazy val fullData = Source.fromFile("../inputs/day4_input.txt").mkString

@main def part1_example() =
  val res = program(exampleData)
  println(s"Result: $res")
  assert(res == 13)

@main def part1() =
  val res = program(fullData)
  println(s"Result: $res")

@main def part2_example() =
  ???
  val res = program(exampleData)
  println(s"Result: $res")
//  assert(res == ???)

@main def part2() =
  ???
  val res = program(fullData)
  println(s"Result: $res")
