package b.long_table

import org.specs2.mutable._

class MainSpec extends Specification {

  sequential

  class context(description: String) extends BeforeAfter {
    var start: Long = 0

    def before = start = System.currentTimeMillis()

    def after = println("[" + description + "] Elapsed: " + (System.currentTimeMillis() - start).toString + " ms")
  }

  "NaiveSolver#solve" should {
    "pass case 1" in new context("Naive:1") {
      NaiveSolver(6, Seq((3, 2), (1, 6), (2, 5))).solve mustEqual Result(4)
    }
    "pass case 2" in new context("Naive:2") {
      NaiveSolver(12, Seq((4, 6), (4, 8), (4, 10), (4, 12), (4, 2), (4, 4))).solve mustEqual Result(12)
    }
    "pass case 3" in new context("Naive:3") {
      NaiveSolver(1, Seq((1, 1))).solve mustEqual Result(1)
    }
    "pass case 4" in new context("Naive:4") {
      NaiveSolver(100, Seq((1, 100)) ++ Seq.fill(99)(100, 1)).solve mustEqual Result(1)
    }
    "pass case 5" in new context("Naive:5") {
      NaiveSolver(100, Seq((1, 100)) ++ Seq.fill(99)(99, 1)).solve mustEqual Result(100)
    }
    "pass case 6 (optional)" in new context("Naive:6") {
      NaiveSolver(1000, Seq((1, 1000)) ++ Seq.fill(999)(1000, 1)).solve mustEqual Result(1)
    }
  }

  "BITSolver#solve" should {
    "pass case 1" in new context("BIT:1") {
      BITSolver(6, Seq((3, 2), (1, 6), (2, 5))).solve mustEqual Result(4)
    }
    "pass case 2" in new context("BIT:2") {
      BITSolver(12, Seq((4, 6), (4, 8), (4, 10), (4, 12), (4, 2), (4, 4))).solve mustEqual Result(12)
    }
    "pass case 3" in new context("BIT:3") {
      BITSolver(1, Seq((1, 1))).solve mustEqual Result(1)
    }
    "pass case 4" in new context("BIT:4") {
      BITSolver(100, Seq((1, 100)) ++ Seq.fill(99)(100, 1)).solve mustEqual Result(1)
    }
    "pass case 5" in new context("BIT:5") {
      BITSolver(100, Seq((1, 100)) ++ Seq.fill(99)(99, 1)).solve mustEqual Result(100)
    }
    "pass case 6 (optional)" in new context("BIT:6") {
      BITSolver(1000, Seq((1, 1000)) ++ Seq.fill(999)(1000, 1)).solve mustEqual Result(1)
    }
    "pass case 7 (optional)" in new context("BIT:7") {
      BITSolver(10000, Seq((1, 10000)) ++ Seq.fill(9999)(10000, 1)).solve mustEqual Result(1)
    }
  }

}
