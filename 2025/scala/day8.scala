package day8

import scala.io.Source

case class Box(x: Int, y: Int, z: Int):
  def distance(a: Box): Double =
    math.sqrt(
      math.pow(x - a.x, 2) + math.pow(y - a.y, 2) + math.pow(z - a.z, 2)
    )
object Box:
  def fromString(s: String): Box =
    val Array(x, y, z) = s.split(',').map(_.toInt)
    Box(x, y, z)

type Circuit = Set[Box]

def connect(circuits: List[Circuit], link: (Box, Box)): List[Circuit] =
  val x = Set(link._1, link._2)
  val (intersecting, distinct) =
    circuits.partition:
      _.intersect(x).nonEmpty
  intersecting.reduce(_ ++ _) :: distinct

def program(input: Iterator[String], limit: Int): Long =
  val boxes = input.map(Box.fromString).toList
  val links = boxes
    .combinations(2)
    .map:
      case List(a, b) => (pair = a -> b, distance = a distance b)
    .toList
    .sortBy(_.distance)

  links
    .take(limit)
    .map(_.pair)
    .foldLeft(boxes.map(Set(_)))(connect)
    .map(_.size)
    .sorted
    .takeRight(3)
    .product

val exampleData =
  """162,817,812
    |57,618,57
    |906,360,560
    |592,479,940
    |352,342,300
    |466,668,158
    |542,29,236
    |431,825,988
    |739,650,466
    |52,470,668
    |216,146,977
    |819,987,18
    |117,168,530
    |805,96,715
    |346,949,466
    |970,615,88
    |941,993,340
    |862,61,35
    |984,92,344
    |425,690,689
    |""".stripMargin.linesIterator

lazy val fullData = Source.fromFile("../inputs/day8_input.txt").getLines()

@main def part1_example() =
  val res = program(exampleData, 10)
  println(s"Result: $res")
  assert(res == 40)

@main def part1() =
  val res = program(fullData, 1000)
  println(s"Result: $res")
