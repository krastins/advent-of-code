import Utils.readStringLines
import scala.annotation.tailrec

val in = readStringLines("2020/day5/input")

def halfway(from: Int, to: Int): Int = from + (to - from) / 2

@tailrec
def findSeat(pass: String, fromRow: Int = 0, toRow: Int = 127, fromCol: Int = 0, toCol: Int = 7): Int = {
  pass.toList match {
    case 'L' :: Nil => fromRow * 8 + fromCol
    case 'R' :: Nil => fromRow * 8 + toCol
    case 'F' :: rest => findSeat(rest.mkString, fromRow, halfway(fromRow, toRow), fromCol, toCol)
    case 'B' :: rest => findSeat(rest.mkString, halfway(fromRow, toRow) + 1, toRow, fromCol, toCol)
    case 'L' :: rest => findSeat(rest.mkString, fromRow, toRow, fromCol, halfway(fromCol, toCol))
    case 'R' :: rest => findSeat(rest.mkString, fromRow, toRow, halfway(fromCol, toCol) + 1, toCol)
  }
}

assert(findSeat("FBFBBFFRLR") == 357)
assert(findSeat("BFFFBBFRRR") == 567)
assert(findSeat("FFFBBBFRRR") == 119)
assert(findSeat("BBFFBBFRLL") == 820)

val seats = in.map(findSeat(_))

def solveFirst(seats: List[Int]): Option[Int] =
  seats.sortBy(- _).headOption

solveFirst(seats)

def solveSecond(seats: List[Int]): Option[Int] = {
  val lookupSet = seats.toSet
  (seats.min to seats.max).find(!lookupSet.contains(_))
}

solveSecond(seats)
