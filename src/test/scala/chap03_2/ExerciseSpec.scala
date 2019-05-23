package chap03_2

import org.scalatest._
import Exercise._

class ExerciseSpec extends FlatSpec with Matchers {

  "Exercise 3-2.2" should "Return number of cells with no bombs in the neighborhood" in {
    val grid: Seq[Seq[CellState]] = Seq(
      Seq(Close, Close, Open(1), Open(3)),
      Seq(Open(0), Close, Open(1), Open(0)),
      Seq(Open(20), Close, Bomb, Bomb),
      Seq(Open(0), Close, Open(2), Open(2))
    )

    noBombsAround(grid) shouldEqual 3
  }

  "Excersize 3-2.3" should "return roots of linear equation using disjoint union" in {
    rootsOfLinearEq((0, 0)) shouldEqual AllX
    rootsOfLinearEq((0, 22)) shouldEqual NoRoot
    rootsOfLinearEq((-12, 24)) shouldEqual Root(2)
  }

  "Exercise 3-2.4" should "return sequence of one roots from a sequence of (a, b) pairs" in {
    val pairs: Seq[(Double, Double)] = Seq(
      (0.0, 0.0),
      (0.0, 1.0),
      (22, 2),
      (3, 33),
      (10, 0)
    )

    solve1(pairs) shouldEqual Seq(-1.0 / 11, -11.0, -0.0)
  }

}
