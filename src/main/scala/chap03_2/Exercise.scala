package chap03_2

object Exercise {

  // 1
  //
  // Represent the state of a Minesweeper game
  // cell.
  // 1. Close
  // 2. Bomb
  // 3. Open containing the number of bombs in the neighbors
  sealed trait CellState
  final case object Close extends CellState
  final case object Bomb extends CellState
  final case class Open(bombs: Int) extends CellState

  def noBombsAround(grid: Seq[Seq[CellState]]): Int = {
    grid.foldLeft(0) { (rprev, rx) =>
      rprev + rx.foldLeft(0) { (prev, x) =>
        x match {
          case Open(0) => prev + 1
          case _       => prev
        }
      }
    }
  }

  // RootOfLinear
  //
  // 1. No roots
  // 2. Exactly one root
  // 3. All x are roots
  sealed trait RootOfLinear
  final case object NoRoot extends RootOfLinear
  final case object AllX extends RootOfLinear
  final case class Root(r: Double) extends RootOfLinear

  def rootsOfLinearEq: ((Double, Double)) => RootOfLinear = {
    case (0, 0) => AllX
    case (0, _) => NoRoot
    case (a, b) => Root(r = -b / a)
  }

  def solve1: Seq[(Double, Double)] => Seq[Double] = pairs => {
    pairs.foldLeft(Seq[Double]()) {
      case (prev, pair) =>
        rootsOfLinearEq(pair) match {
          case Root(r) => prev :+ r
          case _       => prev
        }
    }
  }

  // Defining fully parametric functions
  def f1[A, B]: Option[(A, B)] => (Option[A], Option[B]) = {
    case Some((a, b)) => (Some(a), Some(b))
    case None         => (None, None)
  }

  def f2[A, B]: Either[A, B] => (Option[A], Option[B]) = {
    case Left(a)  => (Some(a), None)
    case Right(b) => (None, Some(b))
  }

  def f3[A, B, C]: Either[A, Either[B, C]] => Either[Either[A, B], C] = {
    case Left(a)         => Left(Left(a))
    case Right(Left(b))  => Left(Right(b))
    case Right(Right(c)) => Right(c)
  }
}
