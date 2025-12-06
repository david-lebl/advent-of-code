package day1_extra

enum Direction:
  case Left, Right

opaque type Distance <: Int = Int
object Distance:
  def apply(value: Int): Distance = value
  def fromStringUnsafe: String => Distance = _.toInt

case class Rotation(direction: Direction, distance: Distance):
  def value: Int = direction match
    case Direction.Left  => distance * -1
    case Direction.Right => distance

object Rotation:
  def fromStringUnsafe: String => Rotation =
    case s"L$x" => Rotation(Direction.Left, Distance.fromStringUnsafe(x))
    case s"R$x" => Rotation(Direction.Right, Distance.fromStringUnsafe(x))

extension (n: Int)
  infix def mod(modulo: Int): Int =
    val res = n % modulo
    if res < 0 then res + modulo else res

case class Dial private (pointer: Int, hits: Int, passes: Int):
  val size = 100
  def rotate: Rotation => Dial = rotate =>
    val movedPointer = (pointer + rotate.value) mod size
    val remaining = rotate.direction match
      case Direction.Left if pointer != 0 => pointer
      case Direction.Right                => size - pointer
      case _                              => size
    val passed =
      if rotate.distance >= remaining then
        (rotate.distance - remaining) / size + 1
      else 0

    Dial(
      pointer = movedPointer,
      hits = if movedPointer == 0 then hits + 1 else hits,
      passes = passes + passed
    )
object Dial:
  def fresh = Dial(50, 0, 0)

def program_part1(input: Iterator[String]) =
  input
    .map(Rotation.fromStringUnsafe)
    .foldLeft(Dial.fresh)(_.rotate(_))
    .hits

def program_part2(input: Iterator[String]) =
  input
    .map(Rotation.fromStringUnsafe)
    .foldLeft(Dial.fresh)(_.rotate(_))
    .passes

@main
def program_part2_test = {
  println:
    List(
      Rotation(Direction.Right, Distance(50))
    )
      .foldLeft(Dial.fresh)(_.rotate(_))
      .passes
}
