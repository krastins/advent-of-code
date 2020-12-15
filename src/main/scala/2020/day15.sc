import scala.annotation.tailrec

val sample = List(0, 3, 6)
val in = List(2, 1, 10, 11, 0, 6)

def init(input: List[Int]): Map[Int, List[Int]] =
  input.zipWithIndex.foldLeft(Map.empty[Int, List[Int]])((history, tup) => history + (tup._1 -> List(tup._2)))

def addToHistory(number: Int, turn: Int, history: Map[Int, List[Int]]): Map[Int, List[Int]] =
  history.get(number) match {
    case Some(v) => history.updated(number, turn :: v)
    case None => history + (number -> List(turn))
  }

def takeTurn(number: Int, turn: Int, history: Map[Int, List[Int]]): (Int, Int, Map[Int, List[Int]]) =
  history.get(number) match {
    case Some(List(_)) | None => (0, turn + 1, addToHistory(0, turn, history))
    case Some(a :: b :: _) =>
      val spokenNumber: Int = a - b
      (spokenNumber, turn + 1, addToHistory(spokenNumber, turn, history))
    }

@tailrec
def playGame(number: Int, turn: Int, history: Map[Int, List[Int]], turns: Int): Int = {
  val (nextNumber, nextTurn, newHistory) = takeTurn(number, turn, history)
  if (turn == turns) number
  else playGame(nextNumber, nextTurn, newHistory, turns)
}

def solveFirst(input: List[Int]): Int = {
  val history = init(input)
  playGame(0, input.size, history, 2020)
}

assert(solveFirst(sample) == 436)
// assert(solveFirst(List(1 ,3, 2)) == 1)
// assert(solveFirst(List(2 ,1, 3)) == 10)
// assert(solveFirst(List(1 ,2, 3)) == 27)
// assert(solveFirst(List(2 ,3, 1)) == 78)
// assert(solveFirst(List(3 ,2, 1)) == 438)
// assert(solveFirst(List(3 ,1, 2)) == 1836)
solveFirst(in)

def solveSecond(input: List[Int]): Int = {
  val history = init(input)
  playGame(0, input.size, history, 30000000)
}

// assert(solveSecond(sample) == 175594)
solveSecond(in)
