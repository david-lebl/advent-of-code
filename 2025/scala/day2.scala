package day2

def isInvalidId(id: Long): Boolean =
  val inStr = id.toString
  val (a, b) = inStr.splitAt(inStr.length / 2)
  a == b

def isInvalidId2(id: Long): Boolean =
  val idS = id.toString
  val n = idS.length
  val dividables = (1 to n / 2).filter(n % _ == 0)
  dividables.exists(d => idS == idS.take(d) * (n / d))

def program(input: String, f: Long => Boolean): Long =
  input
    .split(",")
    .iterator
    .flatMap:
      case s"$a-$b" => a.toLong to b.toLong
    .filter(f)
    .sum

val exampleData =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
val fullData =
  "67562556-67743658,62064792-62301480,4394592-4512674,3308-4582,69552998-69828126,9123-12332,1095-1358,23-48,294-400,3511416-3689352,1007333-1150296,2929221721-2929361280,309711-443410,2131524-2335082,81867-97148,9574291560-9574498524,648635477-648670391,1-18,5735-8423,58-72,538-812,698652479-698760276,727833-843820,15609927-15646018,1491-1766,53435-76187,196475-300384,852101-903928,73-97,1894-2622,58406664-58466933,6767640219-6767697605,523453-569572,7979723815-7979848548,149-216"

@main def example_part1() =
  val res = program(exampleData, isInvalidId)
  println(s"Result: $res") // 1227775554
  assert(res == 1227775554)

@main def part1() =
  val res = program(fullData, isInvalidId)
  println(s"Result: $res") // 54641809925

@main def example_part2() =
  val res = program(exampleData, isInvalidId2)
  println(s"Result: $res") // 4174379265
  assert(res == 4174379265L)

@main def part2() =
  val res = program(fullData, isInvalidId2)
  println(s"Result: $res") // 54641809925
