package day1
import scala.annotation.tailrec
import scala.io.Source

extension (n: Int)
  infix def mod(modulo: Int): Int =
    val res = n % modulo
    if res < 0 then res + modulo else res

@tailrec def dial(pointer: Int, hits: Int = 0, passes: Int = 0)(
    rotations: Seq[Int]
): (hits: Int, passes: Int) =
  rotations match
    case Nil => (hits, passes)
    case x :: xs =>
      // part 1
      val moved = (pointer + x) mod 100
      val hit = if moved == 0 then 1 else 0
      // part 2
      val remaining = if x < 0 && pointer != 0 then pointer else 100 - pointer
      val k = x.abs - remaining
      val passed = if k >= 0 then k / 100 + 1 else 0

      dial(moved, hits + hit, passes + passed)(xs)

def parse: String => Int =
  case s"R$n" => n.toInt
  case s"L$n" => n.toInt * -1

@main def program_part1(input: String): Int =
  dial(pointer = 50):
    input.linesIterator.toList.map(parse)
  .hits

@main def program_part2(input: String): Int =
  dial(pointer = 50):
    input.linesIterator.toList.map(parse)
  .passes

@main def part1_example(): Unit =
  val result = program_part1(exampleData)
  println(s"Password is: $result")

@main def part1(): Unit =
  val result = program_part1(fullData)
  println(s"Password is: $result")

@main def part2_example(): Unit =
  val result = program_part2(exampleData)
  println(s"Password is: $result")

@main def part2(): Unit =
  val result = program_part2(fullData)
  println(s"Password is: $result")

val fullData = Source.fromFile("../inputs/day1_input.txt").mkString
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
    |""".stripMargin
