import Utils.readStringLines

val bagPattern = """(\w+(?: \w+))\sbags\scontain\s(.*)\.""".r
val contentsPattern = """(\d+)\s(\w+(?: \w+))""".r

def accumulateBags(accumulator: Map[String, Int], s: String): Map[String, Int] =
  accumulator ++ (for {
    List(count, color) <- contentsPattern.findFirstMatchIn(s).map(_.subgroups)
  } yield color -> count.toInt)

def parse(accumulator: Map[String, Map[String, Int]], line: String): Map[String, Map[String, Int]] = {
  val bagPattern(color, contents) = line
  val res = if (contents == "no other bags")
    Map.empty[String, Int]
  else
    contents.split(",").foldLeft(Map.empty[String, Int])(accumulateBags)
  accumulator + (color -> res)
}

val in = readStringLines("2020/day7/input")
  .foldLeft(Map.empty[String, Map[String, Int]])(parse)
val sample = readStringLines("2020/day7/sample")
  .foldLeft(Map.empty[String, Map[String, Int]])(parse)

def findEnclosingBags(rules: Map[String, Map[String, Int]])(bag: String): List[String] = {
  val enclosing = rules.filter {
    case (_, v: Map[String, Int]) => v.keys.toSet.contains(bag)
  }.keys.toList
  enclosing ++ enclosing.flatMap(findEnclosingBags(rules))
}

def solveFirst(input: Map[String, Map[String, Int]], bag: String): Int =
  findEnclosingBags(input)(bag).toSet.size

assert(solveFirst(sample, "shiny gold") == 4)
solveFirst(in, "shiny gold")

def countNestedBags(rules: Map[String, Map[String, Int]], bags: Map[String, Int]): Int =
  (for {
    (color, count) <- bags
    nestedBags = rules(color)
  } yield count + count * countNestedBags(rules, nestedBags)).sum

def solveSecond(input: Map[String, Map[String, Int]], bag: String): Int =
  countNestedBags(input, input(bag))

assert(solveSecond(sample, "shiny gold") == 32)
solveSecond(in, "shiny gold")
