package day1

import scala.io.Source

@main def example(): Unit =
  val input = """L68
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
  val result = program(input)
  println(s"Password is: $result")

@main def fullData(): Unit =
  val input = Source.fromFile("../inputs/day1_input.txt").mkString
  val result = program(input)
  println(s"Password is: $result")
