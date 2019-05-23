package chap02
import scala.annotation.tailrec

// Summary
//
// Compute mathematical expressions involving arbitrary recursion
// Use tail recursion when possible
// Use arbitrary inductive (ie. recursive) formulas to:
//      - convert sequences to numbers (sometimes called "aggregate")
//      - create new sequences from scratch
//      - transform existing sequences
//
// Problems like non-tail recursive functions without expression overflow
//      - `accumulator` trick does not always work

object Inductive {

  val label = "Chapter 2 - Induction"

  // Typical Problem
  // Compute a value from a sequence
  //
  // `Seq.sum` and `Seq.max` .. are particular case of such
  // computation but for general cases we need recursion and
  // fold / scan

  // Mathematical formulation
  //
  // 1. Base case -> empty sequence fromDigits(Seq()) = 0
  // 2. Induction step ->
  //        if fromDigits has already computed for a sequence previous...
  //        how to compute it for one more element..
  //        => fromDigits(Seq(previous..., x)) = 10 * fromDigits(previous...) + x

  // recursion requires to code both coses
  def fromDigitsRec(digits: Seq[Int]): Int = {
    if (digits.isEmpty) 0 // base case
    else 10 * fromDigitsRec(digits.tail) + digits.head // inductive case
  }

  // tail recursion to avoid stackoverflow
  //
  // the technique accumulates results through the recursion
  // at the end it returns it.
  @tailrec def fromDigitsTailRec(digits: Seq[Int], res: Int): Int = {
    if (digits.isEmpty) res
    else fromDigitsTailRec(digits.tail, 10 * res + digits.head)
  }

  // Alternative to recursion is foldleft, fold, foldright

  // Foldleft
  //
  // base case doesn't need to be coded in..
  // it encapsulates logic of induxtion
  // it takes as first arg the `base case`
  // and function as `inductive case`
  def fromDigitsFoldLeft(digits: Seq[Int]): Int = {
    digits.foldLeft(0) { case (prev, x) => 10 * prev + x }
  }
  // Computing a sequence from a number~!
  // we can't use map, fold, zip because they require a sequence
  // to begin with
  //
  // we a number which we have to somehow `unfold` it.

  // Inductive definition for a sequence
  //
  // given n > 0, build sequence (mk, dk) until (0,0) or some condition holds
  //
  // (m0, d0) = (n, 0)    -- First element
  // (mk, dk) = (m(k-1)/10, m(k-1 mod 10)) for k>0    -- Subsequent elements
  def seqFromNum(n: Int) = {
    Iterator
      .iterate((n, 0)) { case (m, _) => (m / 10, m % 10) }
      .takeWhile { case (m, d) => m > 0 || d > 0 }
      .drop(1) // doesn't have .tial
      .map(_._2) // _ is a hole ._2 is property name
  }

  // Computing a sequence from another sequence (Scan)
  //
  // Typical problem
  // - partial sums of given sequence: b_k = Sum(i=0 - i=k) ai
  // - Inductive definition
  //      b_0 = 0 (base case)
  //      b_k = a_k + b_(k-1) for k >0 (inductive case) (subsequent case)

  val a = Seq(1, 2, 3)
  val b = a.scan(0) { case (bPrev, ak) => bPrev + ak }
  //             ^                     ^
  //          Base case          Subsequent cases

}
