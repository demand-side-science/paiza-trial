package s.search_island

import org.specs2.mutable._

class MainSpec extends Specification {

  sequential

  class context(description: String) extends BeforeAfter {
    var start: Long = 0

    def before = start = System.currentTimeMillis()

    def after = {
      val elapse = System.currentTimeMillis() - start
      if (start != 0) println("[" + description + "] Elapsed: " + elapse.toString + " ms")
    }
  }

  "Solver" should {
    "pass basic cases" in new context("basic") {
//      Solver(100, 50, Set.empty).solve mustEqual Result(0)
//      Solver(1, 1, Set((0, 0))).solve mustEqual Result(1)
//      Solver(2, 2, Set((0, 0), (0, 1), (1, 0), (1, 1))).solve mustEqual Result(1)
//      Solver(2, 3, Set((0, 0), (0, 1), (1, 1))).solve mustEqual Result(1)
//      Solver(2, 4, Set((0, 0), (1, 1))).solve mustEqual Result(2)
      Solver(Seq(Seq(false))).solve mustEqual Result(0)
      Solver(Seq(Seq(true))).solve mustEqual Result(1)
      Solver(Seq(
        Seq(true, true),
        Seq(true, true)
      )).solve mustEqual Result(1)
      Solver(Seq(
        Seq(false, true),
        Seq(true, false)
      )).solve mustEqual Result(2)
      Solver(Seq(
        Seq(false, true),
        Seq(true, true)
      )).solve mustEqual Result(1)
    }

    val s = (for (i <- 0 until 1000; j <- 0 until 1000) yield (i, j)).toSet
    "pass large cases" in new context("large") {
//      Solver(1000, 1000, s).solve mustEqual Result(1)

      Solver(Seq.fill(1000)(Seq.fill(1000)(false))).solve mustEqual Result(0)
      Solver(Seq.fill(1000)(Seq.fill(1000)(true))).solve mustEqual Result(1)
      Solver(Seq.fill(500)(Seq(
        Seq.fill(500)(Seq(false, true)).flatten,
        Seq.fill(500)(Seq(true, false)).flatten
      )).flatten).solve mustEqual Result(500000)
    }
  }
}
