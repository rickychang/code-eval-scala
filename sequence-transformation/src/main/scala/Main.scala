/**
 * Created by rchang on 3/4/14.
 */

import scala.io.Source
import scala.util.matching.Regex

object Main extends App {

  val digitMap: Map[Char, String] = Map('0' -> "A+", '1' -> "(A+|B+)")

  def genRegEx(digits: String): Regex = {
    val l = digits.map {
      v => digitMap(v)

    }
    "^%s$".format(l.mkString).r
  }
  def isMatch(r: Regex, a: String): String = r.findPrefixOf(a).map(x => "Yes").getOrElse("No")

  val src = Source.fromFile(args(0))
  val lines = src.getLines().filter(_.length > 0)
  for (l <- lines) {
    val Array(digits, alphas) = l.split(" ")
    val regex = genRegEx(digits)
    println(regex)
    val result = isMatch(regex, alphas)
    println(result)
  }

}
