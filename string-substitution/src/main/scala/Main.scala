import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

object Main extends App {

  /*
   * Parse substitutions string into list of pairs
   */
  def parseSubs(subStr: String): List[(String, String)] = {
    subStr.split(",").grouped(2) map (e => (e(0), e(1))) toList
  }

  /*
   * Apply a single substitution to list of segments.  The segment pairs include
   * a boolean value indicating whether or not a substitution was applied already. 
   */
  def makeSub(segments: List[(Boolean, String)], sub: (String, String)): List[(Boolean, String)] = {

    /**
     * Build up substitution results using accumulator and recursive function calls.
     * If match is found, split string at match location and call recursively
     * with remaining string.
     */
    @tailrec
    def helper(acc: ListBuffer[(Boolean, String)], input: String): List[(Boolean, String)] = {
      val matchIndex = input.indexOf(sub._1)
      if (matchIndex == -1) (acc += ((false, input))).filter(e => e._2 != "").toList
      else helper(acc ++ List(((false, input.substring(0, matchIndex))), ((true, sub._2))),
        input.substring(matchIndex + sub._1.length))
    }
    segments.map(e => if (!e._1) helper(new ListBuffer, e._2) else List(e)) flatten
  }

  val source = scala.io.Source.fromFile(args(0))
  val lines = source.getLines.filter(_.length > 0)
  for (l <- lines) {
    val Array(inputStr, subStr) = l.split(";")
    val subs = parseSubs(subStr)
    val res = subs.foldLeft(List((false, inputStr))) { (lb, s) => makeSub(lb, s) }.map(_._2).mkString
    println(res)
  }
}
