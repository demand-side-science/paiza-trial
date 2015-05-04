package d.addition

object Parser {
  def parse: Solver = {
    val Array(a, b) = readLine().split(' ').map(_.toInt)
    Solver(a, b)
  }
}

case class Solver(a: Int, b: Int) {
  def solve = Result(a + b)
}

case class Result(x: Int) {
  def print(): Unit = println(x)
}

object Main extends App {
  Parser.parse.solve.print()
}