package b.long_table

trait Parser {
  def parse: Solver = {
    val Array(n, m) = readLine().split(' ').map(_.toInt)
    val xs = (1 to m).map(_ => readLine().split(' ').map(_.toInt) match {case Array(a, b) => (a, b)})
    BITSolver(n, xs)
  }
}

trait Solver {
  def solve: Result
}

case class NaiveSolver(n: Int, xs: Seq[(Int, Int)]) extends Solver {
  private def range(length: Int, from: Int): Seq[Int] = {
    if (from + length < n)
      Range(from, from + length)
    else
      Range(from, n) ++ Range(0, (from + length) % n)
  }

  @annotation.tailrec
  private def f(used: Seq[Boolean], ls: List[(Int, Int)]): Seq[Boolean] = ls match {
    case (a, b) :: ys if range(a, b - 1).map(used).forall(!_) =>
      f(range(a, b - 1).foldLeft(used) { case (u, x) => u.updated(x, true)}, ys)
    case (a, b) :: ys =>
      f(used, ys)
    case Nil =>
      used
  }

  def solve = {
    val used = f(Seq.fill(n)(false), xs.toList)
    Result(used.count(identity))
  }
}

/** Immutable 0-indexed Binary Index Tree (BIT) */
case class BinaryIndexedTree[A](size: Int, cumulatives: Vector[A])(implicit num: Numeric[A]) {
  require(cumulatives.size == size + 1)

  /** Return the sum of the values indexed from 0 to index (time: O(log(size))) */
  def sum(until: Int): A = {
    @annotation.tailrec
    def f(n: Int, sofar: A): A = if (n == 0) sofar else f(n - (n & -n), num.plus(sofar, cumulatives(n)))

    val i = math.max(0, math.min(size, until))
    f(i, num.zero)
  }

  def sum(from: Int, until: Int): A = num.minus(sum(until), sum(from))

  /** Return the updated BIT (time: O(log(size)))*/
  def updated(index: Int, value: A): BinaryIndexedTree[A] = {
    require((0 until size).contains(index))

    @annotation.tailrec
    def f(n: Int, diff: A, sofar: Vector[A]): Vector[A] =
      if (size < n) sofar else f(n + (n & -n), diff, sofar.updated(n, num.plus(sofar(n), diff)))

    copy(cumulatives = f(index + 1, num.minus(value, get(index)), cumulatives))
  }

  def updated(elems: (Int, A)*): BinaryIndexedTree[A] = elems.foldLeft(this) { case (b, (i, x)) => b.updated(i, x) }

  /** Return the value at the specified index (time: O(log(size))) */
  def get(index: Int): A = sum(index + 1, index)
}

object BinaryIndexedTree {
  def apply[A](size: Int)(implicit num: Numeric[A]): BinaryIndexedTree[A] =
    new BinaryIndexedTree[A](size, Vector.fill(size + 1)(num.zero))
}

case class BITSolver(n: Int, xs: Seq[(Int, Int)]) extends Solver {
  def solve = {
    val res = xs.foldLeft(BinaryIndexedTree[Int](n)) {
      case (bit, (length, from)) =>
        if (bit.sum(from + length - 1 - n) + bit.sum(from, from + length - 1) != 0)
          bit
        else
          bit.updated(((from - 1) until (from + length - 1)).map(_ % n -> 1): _*)
    }
    Result(res.sum(n))
  }
}


case class Result(x: Int) {
  def print(): Unit = println(x)
}

object Main extends App with Parser {
  parse.solve.print()
}