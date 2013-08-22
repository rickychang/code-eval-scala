import scala.annotation.tailrec

object Main extends App {

  val numLetterMap = Map(
    0 -> "0",
    1 -> "1",
    2 -> "abc",
    3 -> "def",
    4 -> "ghi",
    5 -> "jkl",
    6 -> "mno",
    7 -> "pqrs",
    8 -> "tuv",
    9 -> "wxyz")

  def validWords(phoneNum: String): List[String] = {
    val validLetters = phoneNum.toList.map(i => numLetterMap(i.asDigit))

    def helper(l: List[String]): List[String] = l match {
      case List() => List("")
      case hd :: tail => for {
        c <- hd.toList;
        rest <- helper(tail)
      } yield c.toString + rest
    }
    helper(validLetters).sorted
  }

  val source = scala.io.Source.fromFile(args(0))
  val lines = source.getLines.filter(_.length > 0)
  for (l <- lines) {
    println(validWords(l).mkString(","))
  }
}