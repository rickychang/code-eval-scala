import scala.annotation.tailrec

object Main extends App {
  
  def isPalInt(num: Int): Boolean = num.toString == num.toString.reverse
  
  @tailrec
  def reverseAddTest(count: Int, num: Int): (Int, Int) = {
    val numReverse = num.toString.reverse.toInt
    val sum = num + numReverse
    if (isPalInt(sum)) (count + 1, sum)
    else reverseAddTest(count + 1, sum)
  }
  
  val source = scala.io.Source.fromFile(args(0))
  val lines = source.getLines.filter(_.length > 0)
  for (l <- lines) {
    val result = reverseAddTest(0, l.toInt)
    println(result._1 + " " + result._2)
  }

}