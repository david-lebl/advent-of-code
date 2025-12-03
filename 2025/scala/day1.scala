package day1
import scala.annotation.tailrec

@tailrec def decode(dial: Int, zeros: Int)(input: Seq[Int]): Int =
  input match
    case Nil => zeros
    case x :: xs =>
      val moved = (dial + x) % 100
      decode(moved, if moved == 0 then zeros + 1 else zeros)(xs)
def decode(input: Seq[Int]): Int = decode(50, 0)(input)

@main def program(input: String): Int =
  decode:
    input.linesIterator.toList.map:
      case s"R$n" => n.toInt
      case s"L$n" => n.toInt * -1
