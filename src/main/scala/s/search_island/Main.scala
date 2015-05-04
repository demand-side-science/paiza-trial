package s.search_island

trait Parser {
  def parse: Solver = {
    val Array(m, n) = readLine().split(' ').map(_.toInt)
    val xs = for {
      i <- 0 until n
      bs = readLine().split(' ')
      j <- 0 until m if bs(j) == "1"
    } yield {
      (i, j)
    }
    Solver(xs.toSet)
  }
}

case class Result(x: Int) {
  def print(): Unit = println(x)
}

object Main extends App with Parser {
  parse.solve.print()
}

case class Solver(xs: Set[(Int, Int)]) {

  @annotation.tailrec
  private def f(queue: List[(Int, Int)], sofar: Set[(Int, Int)]): Set[(Int, Int)] = queue match {
    case Nil => sofar
    case (x, y) :: t =>
      val q = List((-1, 0), (1, 0), (0, -1), (0, 1)).map { case (dx, dy) => (x + dx, y + dy)}.filter(sofar.contains)
      f(q ::: t, sofar - ((x, y)))
  }

  @annotation.tailrec
  private def g(ys: Set[(Int, Int)], sofar: Int): Int = ys.headOption match {
    case None => sofar
    case Some((x, y)) => g(f(List((x, y)), ys), sofar + 1)
  }

  def solve: Result = {
    Result(g(xs, 0))
  }
}




case class Solver2(xss: Seq[Seq[Boolean]]) {
  val n = xss.length
  val m = xss.head.length

  private def isBlack(yss: Seq[Seq[Boolean]], x: Int, y: Int): Boolean =
    (0 until n).contains(x) && (0 until m).contains(y) && yss(x)(y)

  @annotation.tailrec
  private def f(queue: List[(Int, Int)], sofar: Seq[Seq[Boolean]]): Seq[Seq[Boolean]] = queue match {
    case Nil =>
      sofar
    case (x, y) :: t =>
      val nq = List((-1, 0), (1, 0), (0, -1), (0, 1))
        .map { case (dx, dy) => (x + dx, y + dy)}
        .filter { case (nx, ny) => isBlack(sofar, nx, ny)}
      f(t ::: nq, sofar.updated(x, sofar(x).updated(y, false)))
  }

  private def getNext(x: Int, y: Int): (Int, Int) = if (y == m - 1) (x + 1, 0) else (x, y + 1)

  @annotation.tailrec
  private def g(yss: Seq[Seq[Boolean]], xy: (Int, Int), sofar: Int): Int = xy match {
    case (x, _) if x >= n =>
      sofar
    case (x, y) if isBlack(yss, x, y) =>
      g(f(List((x, y)), yss), getNext(x, y), sofar + 1)
    case (x, y) =>
      g(yss, getNext(x, y), sofar)
  }

  def solve: Result = {
    Result(g(xss, (0, 0), 0))
  }
}
