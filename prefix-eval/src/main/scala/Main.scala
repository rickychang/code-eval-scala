import scala.collection.mutable.Stack

object Main extends App {
  
  val operators = Map(
      "+" -> { (x: Int, y: Int) => x + y },
      "-" -> { (x: Int, y: Int) => x - y },
      "*" -> { (x: Int, y: Int) => x * y },
      "/" -> { (x: Int, y: Int) => x / y })
  
  def isOperator(s: String): Boolean = operators contains s
  
  def eval(prefixStr: String): Int = {
    val stack = new Stack[Int]
    val reversed = prefixStr.split(" ").reverse
    for (v <- reversed) {
      if (isOperator(v)) {
        val op1 = stack.pop
        val op2 = stack.pop
        val operator = operators(v)
        val result = operator(op1, op2)
        stack.push(result)
      } else {
        stack.push(v.toInt)
      }
    }
    stack.pop
  }

  val source = scala.io.Source.fromFile(args(0))
  val lines = source.getLines.filter(_.length > 0)
  for (l <- lines) {
   	println(eval(l))
  }
}