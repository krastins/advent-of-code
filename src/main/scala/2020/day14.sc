import Utils.readStringLines

def parseInput(input: List[String]): List[List[String]] =
  List.unfold(input) {
    case Nil => None
    case x :: as =>
      as.span(!_.startsWith("mask =")) match {
        case (prefix, Nil) => Some(x :: prefix, List.empty)
        case (prefix, suffix) => Some(x :: prefix, suffix)
      }
  }

val in = parseInput(readStringLines("2020/day14/input"))
val sample1 = parseInput(readStringLines("2020/day14/sample1"))
val sample2 = parseInput(readStringLines("2020/day14/sample2"))

val writePattern = """mem\[(\d+)\] = (\d+)""".r

final case class Write(address: Int, value: Int)

def parseWrite(write: String) = {
  val writePattern(address, value) = write
  Write(address.toInt, value.toInt)
}

def paddedBinary(number: Long): String =
  number.toBinaryString.reverse.padTo(36, '0').reverse

def parseBits(bits: String): Long =
  java.lang.Long.parseLong(bits, 2)

def process(program: List[String], memory: Map[Int, Long] = Map[Int, Long]()): Map[Int, Long] = {
  val rawMask :: rawWrites = program
  val mask = rawMask.split(" = ").last
  val writes = rawWrites.map(parseWrite)
  writes.foldLeft(memory)((memory, write) => {
    val number: String = paddedBinary(write.value)
    val res: Long = parseBits(number.zip(mask).map { case (numberBit, maskBit) => maskBit match {
      case 'X' => numberBit
      case _ => maskBit
    }}.mkString)
    memory + (write.address -> res)
  })
}

def solveFirst(input: List[List[String]]): Long =
  input.foldLeft(Map[Int, Long]())((memory, program) => {
    process(program, memory)
  }).values.sum

assert(solveFirst(sample1) == 165)
solveFirst(in)

def writeToAddresses(write: Write, mask: String): Map[Long, Long] = {
  val number: String = paddedBinary(write.address)
  val addresses: Seq[List[Int]] = number.zip(mask).map { case (numberBit, maskBit) =>
    maskBit match {
      case 'X' => List(0, 1)
      case '1' => List(1)
      case '0' => List(Integer.parseInt(numberBit.toString))
    }
  }

  def buildAddresses(result: List[String], bits: Seq[Int]): List[String] = for {
    aa <- result
    bb <- bits
  } yield aa + bb

  addresses.foldLeft(List[String](""))(buildAddresses)
    .map(parseBits)
    .foldLeft(Map[Long, Long]())((as, a) => as + (a -> write.value))
}

def process2(program: List[String], memory: Map[Long, Long] = Map[Long, Long]()): Map[Long, Long] = {
  val rawMask :: rawWrites = program
  val mask = rawMask.split(" = ").last
  val writes = rawWrites.map(parseWrite)
  writes.foldLeft(memory)((mem, write) => mem ++ writeToAddresses(write, mask))
}

def solveSecond(input: List[List[String]]): Long =
  input.foldLeft(Map[Long, Long]())((memory, program) => {
    process2(program, memory)
  }).values.sum

assert(solveSecond(sample2) == 208)
solveSecond(in)
