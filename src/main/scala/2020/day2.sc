import Utils.readStringInput

val in = readStringInput("2020/day2/input")
  .map(_.replaceAll(":", "").split("\\s|-"))

val sample = List(
  Array("1", "3", "a", "abcde"),
  Array("1", "3", "b", "cdefg"),
  Array("2", "9", "c", "ccccccccc")
)

def isValidFirst(policy: Array[String]): Boolean = policy match {
  case Array(from, to, char, password) =>
    val occurrences = password.count(char.headOption.contains)
    occurrences >= from.toInt && occurrences <= to.toInt
}

def solveFirst(input: Seq[Array[String]]): Int =
  input.count(isValidFirst)

assert(solveFirst(sample) == 2)
solveFirst(in)

def isValidSecond(policy: Array[String]): Boolean = policy match {
  case Array(from, to, char, password) =>
    password.lift(from.toInt - 1) == char.headOption ^
      password.lift(to.toInt - 1) == char.headOption
}

def solveSecond(input: Seq[Array[String]]): Int =
  input.count(isValidSecond)

assert(solveSecond(sample) == 1)
solveSecond(in)
