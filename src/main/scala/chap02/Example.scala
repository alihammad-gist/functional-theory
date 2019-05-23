package chap02
import scala.annotation.tailrec

object Example {

  // 1
  //
  // Compute the smallest n sucht that f(f(f(...f(1)...))) > 1000,
  // where f is applied n times recursively (f(n) = 2n + 1)
  def ex1(fn: Int => Int, n: Int, limit: Int): Int = {
    Iterator.iterate(n)(fn).takeWhile(_ < limit).size
  }

  // 2
  //
  // kth largest element in a sequence of unsorted elements
  def ex2(s: Seq[Int], k: Int): Int = {
    val sorted = Seq.fill(k)(Int.MinValue)

    s.foldLeft(sorted) {
        case (seq, x) =>
          (seq :+ x)
            .sorted(Ordering[Int].reverse)
            .take(k)
      }
      .last
  }

  // 3
  //
  // find last element of a sequence tial recursion.
  @tailrec def ex3[A](s: Seq[A]): A = s match {
    // case Seq()  isn't covered
    case Seq(x) => x
    case _      => ex3(s.tail)
  }

  // 4
  //
  // binary search using tail recursion
  def ex4(s: Array[Int], a: Int): Int = s match {
    case Array(x) => x
    case _ =>
      val (l, r) = s.splitAt(s.length / 2)
      if (s((s.length / 2)) > a) ex4(l, a)
      else ex4(r, a)
  }

  // 5
  //
  // computer the sequence s given an int n,
  // such s_0 = SD(n) and s_k = SD(s_(k-1) for k >1
  // where, SD(p) is the sum of decimal digits of int p,
  // eg, SD(123) = 6
  def ex5(num: Int): Iterator[Int] = {

    def sumDigits(n: Int): Int = {

      val items = Iterator
        .iterate((n / 10, n % 10)) {
          case (m, n) => (m / 10, n + m % 10)
        }
        .takeWhile { case (m, n) => m > 0 }
        .toList

      if (items.length > 0) items.last._1 + items.last._2
      else 0
    }

    Iterator.iterate(sumDigits(num))(sumDigits)
  }

  // 6
  //
  // compute 'half-speed' sequence ie. (s0, s1, s2, ..) => (s0, s0, s1, s1, s2, s2, ...)
  def ex6[A](s: Iterator[A]): Iterator[A] = {
    s.flatMap(x => Iterator.fill(2)(x))
  }

  // 7
  //
  // cut off a sequence where it begins to repeat a sub sequence
  def ex07(s: Seq[Int]): Seq[Int] = {
    // s0, s1, s2, s3, s4
    // s0, s0, s1, s1, s2, s2, s3, s3, s4, s4    - half speed
    // _ (s1, so) (s2, s1) (s3, s1)
    Seq(s.head) ++ s.toIterator
      .zip(ex6(s.toIterator))
      .drop(1)
      .takeWhile({
        case (ii, i) => ii != i
      })
      .map(_._1)
      .toSeq
  }
}
