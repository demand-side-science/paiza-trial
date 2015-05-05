package s.search_island

import scala.collection.mutable

trait Parser {
  def parse: Solver = {
    val Array(m, n) = readLine().split(' ').map(_.toInt)
    val xss = (1 to n).map(_ => readLine().split(' ').map(_ == "1").toSeq)
    Solver(xss)
  }
}

case class Result(x: Int) {
  def print(): Unit = println(x)
}

object Main extends App with Parser {
  parse.solve.print()
}

case class Solver(xss: Seq[Seq[Boolean]]) {
  val n = xss.length
  val m = xss.head.length

  val xs = new mutable.ArrayBuffer[Boolean] ++= xss.flatten

  private def isBlack(x: Int, y: Int): Boolean =
    (0 until n).contains(x) && (0 until m).contains(y) && xs(x * m + y)

  @annotation.tailrec
  private def f(queue: List[(Int, Int)]): Unit = queue match {
    case Nil =>
    case (x, y) :: t =>
      val q = List((-1, 0), (1, 0), (0, -1), (0, 1))
        .map { case (dx, dy) => (x + dx, y + dy)}
        .filter { case (nx, ny) => isBlack(nx, ny)}
      xs(x * m + y) = false
      f(q ::: t)
  }

  @annotation.tailrec
  private def g(i: Int, sofar: Int): Int =
    if (i >= n * m) {
      sofar
    } else if (xs(i)) {
      f(List((i / m, i % m)))
      g(i + 1, sofar + 1)
    } else {
      g(i + 1, sofar)
    }

  def solve: Result = {
    Result(g(0, 0))
  }
}
