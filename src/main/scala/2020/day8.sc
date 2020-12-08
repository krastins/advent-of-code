import Utils.readStringLines

import scala.annotation.tailrec

final case class Op(instruction: String, argument: Int)
final case class Result(terminated: Boolean, value: Int)

def parseInstructions(lines: List[String]): Vector[Op] =
  lines.map(line => line.split("\\s") match {
    case Array(instruction, arg) => Op(instruction, arg.toInt)
  }).toVector

val in = parseInstructions(readStringLines("2020/day8/input"))
val sample = parseInstructions(readStringLines("2020/day8/sample"))

@tailrec
def execute(instructions: IndexedSeq[Op], pos: Int = 0, acc: Int = 0, executed: Set[Int] = Set.empty): Result =
  if (executed.contains(pos)) Result(terminated = false, acc)
  else if (pos == instructions.size) Result(terminated = true, acc)
  else
    instructions(pos) match {
      case Op("acc", arg) => execute(instructions, pos + 1, acc + arg, executed + pos)
      case Op("jmp", arg) => execute(instructions, pos + arg, acc, executed + pos)
      case Op("nop", _) => execute(instructions, pos + 1, acc, executed + pos)
    }

assert(execute(sample) == Result(terminated = false, 5))
execute(in)

def swapInstruction(instructions: IndexedSeq[Op], pos: Int): Option[IndexedSeq[Op]] =
  instructions(pos) match {
    case Op("jmp", arg) => Some(instructions.updated(pos, Op("nop", arg)))
    case Op("nop", arg) => Some(instructions.updated(pos, Op("jmp", arg)))
    case Op(_, _) => None
  }

@tailrec
def solveSecond(instructions: IndexedSeq[Op], pos: Int = 0): Result =
  swapInstruction(instructions, pos).map(execute(_)) match {
    case Some(Result(false, _)) => solveSecond(instructions, pos + 1)
    case Some(r @ Result(true, _)) => r
    case None => solveSecond(instructions, pos + 1)
  }

assert(solveSecond(sample) == Result(terminated = true, 8))
solveSecond(in)
