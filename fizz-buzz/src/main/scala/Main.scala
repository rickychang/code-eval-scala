
import scala.collection.mutable.ListBuffer
import scala.collection.breakOut

object Main extends App {

  def getFizzBuzz(a: Int, b: Int, n: Int): List[String] = {
    (1 to n).toList map { i: Int =>
      i match {
        case v if v % a == 0 && v % b == 0 => "FB"
        case v if v % a == 0 => "F"
        case v if v % b == 0 => "B"
        case _ => i.toString
      }
    }
  }

  val source = scala.io.Source.fromFile(args(0))
  val lines = source.getLines.filter(_.length > 0)
  for (l <- lines) {
    l.split(" ") match {
      case Array(a, b, n) => println(getFizzBuzz(a.toInt, b.toInt, n.toInt).mkString(" "))
    }
  }
}