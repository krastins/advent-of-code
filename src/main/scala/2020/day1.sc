import Utils.{readIntInput, time}

val in = readIntInput("2020/day1/input")

val sample: List[Int] = List(1721, 979, 366, 299, 675, 1456)

def solveFirstNaive(input: List[Int]): Option[Int] =
  input.combinations(2).find {case List(a, b) => a + b == 2020 } map {case List(a, b) => a * b}

def solveFirstOptimized(n: Int, input: List[Int]): Option[Int] = {
  val inputSet = input.toSet
  input.collectFirst { case i if inputSet.contains(n - i) => i * (n - i) }
}

assert(solveFirstNaive(sample).contains(514579))
assert(solveFirstOptimized(2020, sample).contains(514579))

time(solveFirstNaive(in))
time(solveFirstOptimized(2020, in))

def solveSecondNaive(input: List[Int]): Option[Int] =
  input.combinations(3)
    .find { case List(a, b, c) => a + b + c == 2020 }
    .map { case List(a, b, c) => a * b * c }

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
