package day4

import scala.annotation.tailrec
import scala.io.Source

type Grid = Array[Array[Char]]
extension (grid: Grid)
  def isDefinedAt(x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && y < grid.length && x < grid(y).length
  def updatedAt(x: Int, y: Int)(c: Char): Grid =
    grid.updated(y, grid(y).updated(x, c))

object Grid:
  def fromString(s: String): Grid =
    s.linesIterator.map(_.toCharArray).toArray

def findNeighbours(grid: Grid): Seq[(x: Int, y: Int, cnt: Long)] =
  for
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
  yield (x = x, y = y, cnt = papers.count(_ == '@'))

def program_part1(grid: Grid): Long = findNeighbours(grid).count(_.cnt < 4)
def program_part1(matrix: String): Long = program_part1(Grid.fromString(matrix))

def program_part2(matrix: String): Long =
  program_part2(Grid.fromString(matrix), 0)
@tailrec
def program_part2(grid: Grid, totalCnt: Int): Long =
  val removing = findNeighbours(grid).filter(_.cnt < 4)
  if removing.isEmpty then totalCnt
  else
    val updated = removing.foldLeft(grid): (g, r) =>
      g.updatedAt(r.x, r.y)('.')
    program_part2(updated, totalCnt + removing.size)

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
  val res = program_part1(exampleData)
  println(s"Result: $res")
  assert(res == 13)

@main def part1() =
  val res = program_part1(fullData)
  println(s"Result: $res")

@main def part2_example() =
  val res = program_part2(exampleData)
  println(s"Result: $res")
  assert(res == 43)

@main def part2() =
  val res = program_part2(fullData)
  println(s"Result: $res")
