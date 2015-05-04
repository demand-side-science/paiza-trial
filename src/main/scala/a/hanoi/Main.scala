package a.hanoi

trait Parser {
  def parse: Solver = {
    val Array(n, t) = readLine().split(' ').map(_.toInt)
    Solver(n, t)
  }
}

case class Solver(n: Int, t: Int) {
  @annotation.tailrec
  private def f(pos: Int, whenOff: Int, whenOn: Int, another: Int, sofar: Seq[List[Int]]): Seq[List[Int]] =
    if (pos == 0) {
      sofar.map(_.reverse)
    } else if (((t >> (pos - 1)) & 1) == 1) {
      f(pos - 1, another, whenOn, whenOff, sofar.updated(whenOn, pos :: sofar(whenOn)))
    } else {
      f(pos - 1, whenOff, another, whenOn, sofar.updated(whenOff, pos :: sofar(whenOff)))
    }


  def solve: Result = {
    Result(f(n, 0, 2, 1, Seq.fill(3)(List.empty)))
  }
}

case class Result(abc: Seq[List[Int]]) {
  def print(): Unit = abc.map {
    case Nil => "-"
    case xs => xs.mkString(" ")
  }.foreach(println)
}

object Main extends App with Parser {
  parse.solve.print()
}