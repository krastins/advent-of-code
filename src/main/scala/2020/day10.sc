import Utils.{readIntLines, time}

import scala.annotation.tailrec

val in = readIntLines("2020/day10/input").sorted
val sample1 = readIntLines("2020/day10/sample1").sorted
val sample2 = readIntLines("2020/day10/sample2").sorted

def findAdapter(where: List[Int], jolts: Int): Option[Int] =
  where.find(i => i == jolts + 1 || i == jolts + 3)

@tailrec
def differences(in: List[Int], jolts: Int = 0, acc: List[Int] = List()): List[Int] =
  findAdapter(in, jolts) match {
    case Some(nextJolts) => differences(in, nextJolts, nextJolts - jolts :: acc)
    case None => 3 :: acc
  }

def solveFirst(input: List[Int]): Int =
  differences(input).groupBy(identity).values.map(_.size).product

assert(solveFirst(sample1) == 35)
assert(solveFirst(sample2) == 220)
solveFirst(in)

def solveSecond(input: List[Int]): Option[Long] =
  (0 :: input).map(jolts => input.count((jolts + 1 to jolts + 3).contains))
    .foldRight(List.empty[Long])((current, acc) => current match {
      case 0 => 1 :: acc
      case n => acc.take(n).sum :: acc
    }).headOption

assert(solveSecond(sample1).contains(8))
assert(solveSecond(sample2).contains(19208))
time(solveSecond(in))
