import scala.io.{BufferedSource, Source}
import java.util.concurrent.TimeUnit

object Utils {
  def readIntLines(filename: String): List[Int] = {
    val bufferedSource: BufferedSource = Source.fromResource(filename)

    val lines: Iterator[String] = bufferedSource.getLines()

    val ints = lines.map(line => line.toInt).toList
    bufferedSource.close
    ints
  }

  def readStringLines(filename: String): List[String] = {
    val bufferedSource: BufferedSource = Source.fromResource(filename)

    val lines: List[String] = bufferedSource.getLines().toList
    bufferedSource.close
    lines
  }

  def readString(filename: String): String = {
    val bufferedSource: BufferedSource = Source.fromResource(filename)

    val lines: String = bufferedSource.getLines().mkString("\n")
    bufferedSource.close
    lines
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + TimeUnit.NANOSECONDS.toMillis(t1 - t0) + "ms")
    result
  }
}
