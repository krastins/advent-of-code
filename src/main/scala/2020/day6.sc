import Utils.readString

val in = readString("2020/day6/input")
val sample = readString("2020/day6/sample")

def solveFirst(input: String): Int =
  input.split("\n\n")
    .map(_.replaceAll("[\n\\s]", "").toSet.size)
    .sum

assert(solveFirst(sample) == 11)
solveFirst(in)

def countQuestionIntersections(s: String): Int =
  s.split("\n")
    .map(_.toSet)
    .reduce((a, b) => a.intersect(b))
    .size

def solveSecond(input: String): Int =
  input.split("\n\n").map(countQuestionIntersections).sum

assert(solveSecond(sample) == 6)
solveSecond(in)
