import Utils.readStringLines
import scala.math.abs

val in = readStringLines("2020/day12/input")

val sample = List("F10", "N3", "F7", "R90", "F11")

object Direction extends Enumeration {
  type Direction = Value
  val North = Value(0)
  val East = Value(90)
  val South = Value(180)
  val West = Value(270)
}

import Direction._

case class Position(eastWest: Int, northSouth: Int, facing: Direction)

def moveForward(pos: Position, amount: Int): Position =
  pos.facing match {
    case North => pos.copy(northSouth = pos.northSouth + amount)
    case East => pos.copy(eastWest = pos.eastWest + amount)
    case South => pos.copy(northSouth = pos.northSouth - amount)
    case West => pos.copy(eastWest = pos.eastWest - amount)
  }

def turn(pos: Position, amount: Int): Position =
  pos.copy(facing = Direction((pos.facing.id + amount + 360) % 360))

def parseInstruction(instructions: String): (String, Int) = {
  val (action, unparsedAmount) = instructions.splitAt(1)
  (action, unparsedAmount.toInt)
}

def updatePosition(pos: Position, instructions: String): Position = {
  val (action, amount) = parseInstruction(instructions)
  action match {
    case "N" => pos.copy(northSouth = pos.northSouth + amount)
    case "S" => pos.copy(northSouth = pos.northSouth - amount)
    case "E" => pos.copy(eastWest = pos.eastWest + amount)
    case "W" => pos.copy(eastWest = pos.eastWest - amount)
    case "L" => turn(pos, -amount)
    case "R" => turn(pos, amount)
    case "F" => moveForward(pos, amount)
  }
}

def solveFirst(input: List[String]): Int = {
  val Position(eastWest, northSouth, _) = input.foldLeft(Position(0, 0, East))(updatePosition)
  abs(eastWest) + abs(northSouth)
}

assert(solveFirst(sample) == 25)
solveFirst(in)

case class Waypoint(eastWest: Int, northSouth: Int)

def moveWaypoint(pos: Position, wp: Waypoint, amount: Int): Position =
  pos.copy(eastWest = pos.eastWest + (wp.eastWest * amount),
    northSouth = pos.northSouth + (wp.northSouth * amount))

def turnWaypoint(wp: Waypoint, amount: Int): Waypoint = amount match {
  case 90 | -270 => wp.copy(eastWest = wp.northSouth, northSouth = -wp.eastWest)
  case 180 | -180 => wp.copy(eastWest = -wp.eastWest, northSouth = -wp.northSouth)
  case 270 | -90 => wp.copy(eastWest = -wp.northSouth, northSouth = wp.eastWest)
}

def updatePositionWithWaypoint(pw: (Position, Waypoint), instructions: String): (Position, Waypoint) = {
  val (pos, wp) = pw
  val (action, amount) = parseInstruction(instructions)
  action match {
    case "N" => (pos, wp.copy(northSouth = wp.northSouth + amount))
    case "S" => (pos, wp.copy(northSouth = wp.northSouth - amount))
    case "E" => (pos, wp.copy(eastWest = wp.eastWest + amount))
    case "W" => (pos, wp.copy(eastWest = wp.eastWest - amount))
    case "L" => (pos, turnWaypoint(wp, -amount))
    case "R" => (pos, turnWaypoint(wp, amount))
    case "F" => (moveWaypoint(pos, wp, amount), wp)
  }
}

def solveSecond(input: List[String]): Int = {
  val start = (Position(0, 0, East), Waypoint(10, 1))
  val (Position(eastWest, northSouth, _), _) = input.foldLeft(start)(updatePositionWithWaypoint)
  abs(eastWest) + abs(northSouth)
}

assert(solveSecond(sample) == 286)
solveSecond(in)
