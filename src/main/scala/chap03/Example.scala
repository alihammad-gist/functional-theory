package chap03

import FunctionType._
import scala.annotation.tailrec

object Example {

  def ex01[X, T]: X => T => T = const(id)

  def twice[A]: (A => A) => (A => A) =
    f => x => f(f(x))

  def applyN[A]: (A => A, Int) => (A => A) =
    (f, n) =>
      x =>
        Iterator
          .iterate(true)(_ => true)
          .take(n)
          .foldLeft(x) { (prev, _) =>
            f(prev)
          }

  def applyNRec[A]: (A => A, Int) => (A => A) =
    (f, n) => {
      @tailrec
      def rec(n: Int, x: A): A =
        if (n < 1) x
        else rec(n - 1, f(x))

      (x: A) => rec(n, x)
    }

  def converge[X](f: X => X, x: X, pred: X => Boolean): X = {
    Iterator
      .iterate(x)(f)
      .takeWhile(pred)
      .toSeq
      .last
  }

  def convergeRec[X](f: X => X, x0: X, pred: X => Boolean): X = {
    @tailrec
    def rec(x: X): X = {
      if (!pred(x)) x
      else rec(f(x))
    }

    rec(x0)
  }

  val a: Seq[Int] = Seq(1,2,3)
  a.map(x => x + 1)

}
