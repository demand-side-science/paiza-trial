package s.mod7

trait Parser {
  def parse: Solver = {
    val n = readLine().toInt
    val xs = (1 to n).map(_ => readLine().toLong)
    Solver(xs)
  }
}

case class Result(x: Long) {
  def print(): Unit = println(x)
}

object Main extends App with Parser {
  parse.solve.print()
}

case class Solver(xs: Seq[Long]) {
  val n = xs.length

  def solve: Result = {
    val counts: Map[Int, Long] =
      (0 until 7).map(_ -> 0L).toMap ++ xs.groupBy(x => (x % 7).toInt).mapValues(_.length.toLong)
    val zs = for {
      a <- 0 until 7
      b <- 0 until 7
    } yield {
      val c = (14 - a - b) % 7
      val db = if (a == b) 1 else 0
      val dc = if (a == c && b == c) 2 else if (a == c || b == c) 1 else 0
      counts(a) * (counts(b) - db) * (counts(c) - dc)
    }

    Result(zs.sum / 6)
  }
}
