import Utils.{readIntLines, time}

val in = readIntLines("2020/day1/input")

val sample: List[Int] = List(1721, 979, 366, 299, 675, 1456)

def solveFirstNaive(input: List[Int]): Option[Int] =
  input.combinations(2).find {case List(a, b) => a + b == 2020 }.map(_.product)


def solveFirstForComprehensions(input: List[Int]): Option[Int] = {
  val res: List[Int] = for {
    i <- input
    j <- input
    if i + j == 2020
  } yield i * j
  res.headOption
}

def solveFirstOptimized(input: List[Int]): Option[Int] = {
  val inputSet = input.toSet
  input.map { case i if inputSet.contains(2020 - i) => i * (2020 - i) }.headOption
}

assert(solveFirstNaive(sample).contains(514579))
assert(solveFirstForComprehensions(sample).contains(514579))
assert(solveFirstOptimized(2020, sample).contains(514579))

time(solveFirstNaive(in))
time(solveFirstForComprehensions(in))
time(solveFirstOptimized(2020, in))

def solveSecondNaive(input: List[Int]): Option[Int] =
  input.combinations(3)
    .find { case List(a, b, c) => a + b + c == 2020 }
    .map(_.product)

def solveSecondOptimized(input: List[Int]): Option[Int] = {
  input.collectFirst(e =>
    solveFirstOptimized(2020 - e, input) match {
      case Some(v) => v * e
    }
  )
}

assert(solveSecondNaive(sample).contains(241861950))
assert(solveSecondOptimized(sample).contains(241861950))

time(solveSecondNaive(in))
time(solveSecondOptimized(in))
