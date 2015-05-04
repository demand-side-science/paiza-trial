package d.sort_number

object Parser {
  def parse: Solver = {
    val n = readLine().toInt
    val xs = (1 to n).map(_ => readLine().toInt)
    Solver(xs)
  }
}

case class Solver(xs: Seq[Int]) {
  def solve = Result(xs.sorted)
}

case class Result(xs: Seq[Int]) {
  def print(): Unit = xs foreach println
}

object Main extends App {
  Parser.parse.solve.print()
}