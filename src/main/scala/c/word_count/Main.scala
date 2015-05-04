package c.word_count

object Parser {
  def parse: Solver = {
    val xs = readLine().split(' ')
    Solver(xs)
  }
}

case class Solver(xs: Seq[String]) {
  def solve = Result(xs.map(x => x -> xs.count(_ == x)).distinct)
}

case class Result(xs: Seq[(String, Int)]) {
  def print(): Unit = xs foreach { case (s, i) => println(s + " " + i.toString) }
}

object Main extends App {
  Parser.parse.solve.print()
}