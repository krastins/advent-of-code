import Utils.readStringLines

val in = readStringLines("2020/day2/input")
  .map(_.replaceAll(":", "").split("\\s|-"))

val sample = List(
  Array("1", "3", "a", "abcde"),
  Array("1", "3", "b", "cdefg"),
  Array("2", "9", "c", "ccccccccc")
)

def isValidFirst(policy: Array[String]): Boolean = policy match {
  case Array(from, to, char, password) =>
    val occurrences = password.count(char.headOption.contains)
    from.toIntOption.exists(occurrences >= _) &&
      to.toIntOption.exists(occurrences <= _)
}

def solveFirst(input: Seq[Array[String]]): Int =
  input.count(isValidFirst)

assert(solveFirst(sample) == 2)
solveFirst(in)

def isValidSecond(policy: Array[String]): Boolean = policy match {
  case Array(from, to, char, password) =>
    from.toIntOption.map(_ - 1).flatMap(password.lift) == char.headOption ^
      to.toIntOption.map(_ - 1).flatMap(password.lift) == char.headOption
}

def solveSecond(input: Seq[Array[String]]): Int =
  input.count(isValidSecond)

assert(solveSecond(sample) == 1)
solveSecond(in)
