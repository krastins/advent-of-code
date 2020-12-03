import Utils.readStringInput
import scala.annotation.tailrec

val in = readStringInput("2020/day3/input")

val sample = List(
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"
)

def moveRight(steps: Int)(line: String): String =
  line.concat(line).slice(steps, line.length + steps)

def countTopLeftTrees(grid: List[String]): Int = {
  grid.headOption.flatMap(_.headOption).count(_ == '#')
}

@tailrec
def countTrees(grid: List[String], x: Int, y: Int, treeCount: Int = 0): Int = {
  if (grid.isEmpty)
    treeCount
  else {
    val newPosition = grid.map(moveRight(x)).drop(y)
    countTrees(newPosition, x, y, treeCount + countTopLeftTrees(newPosition))
  }
}

def solveFirst(grid: List[String]): Int =
  countTrees(grid, 3, 1)

assert(solveFirst(sample) == 7)
solveFirst(in)

def solveSecond(grid: List[String]): Int =
  List(
    countTrees(grid, 1, 1),
    countTrees(grid, 3, 1),
    countTrees(grid, 5, 1),
    countTrees(grid, 7, 1),
    countTrees(grid, 1, 2)
  ).product

assert(solveSecond(sample) == 336)
solveSecond(in)
