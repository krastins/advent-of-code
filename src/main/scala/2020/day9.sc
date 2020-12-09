import Utils.{readLongLines, time}
import scala.annotation.tailrec

val in = readLongLines("2020/day9/input")
val sample = List[Long](35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)

def validNextNumber(previous: List[Long], current: Long): Boolean =
  previous.combinations(2).map(_.sum).contains(current)

@tailrec
def solveFirst(input: List[Long], range: Int): Option[Long] =
  if (input.size < range) None
  else if (validNextNumber(input.slice(0, range), input(range)))
    solveFirst(input.tail, range)
  else
    Some(input(range))

assert(solveFirst(sample, 5).contains(127))
time(solveFirst(in, 25))

@tailrec
def solveSecond(input: List[Long], lookup: Long, range: Int=2): Option[Long] =
  input.sliding(range).filter(_.sum == lookup).nextOption match {
    case Some(l) => Some(l.min + l.max)
    case None => solveSecond(input, lookup, range + 1)
  }

assert(solveSecond(sample, 127).contains(62))
time(solveSecond(in, 217430975))
