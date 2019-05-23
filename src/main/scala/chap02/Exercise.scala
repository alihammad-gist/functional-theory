package chap02

import libs.CartesianProduct

object Exercise {

  // 1
  //
  // Compute sum of squared digits
  def xr1(num: Int): Int = {
    val pairs = Iterator
      .iterate((num / 10, (num % 10) * (num % 10))) {
        case (m, n) => (m / 10, n + (m % 10) * (m % 10))
      }
      .takeWhile {
        case (m, _) => m > 0
      }
      .toList

    if (pairs.length > 0) pairs.last._1 * pairs.last._1 + pairs.last._2
    else 0
  }

  // 2
  //
  // given an n, sum of cubed digits, then sum of cubed digits on result.
  // f(n) = sum of cubed digits
  // Sequence = f(f(...f(n)...))
  //
  // check whether sequence repeats itself
  def xr2(num: Int): List[Int] = {
    val sumOfCubes = (n: Int) =>
      Inductive
        .seqFromNum(n)
        .toList
        .fold(0) { (prev, x) =>
          prev + (x * x * x) // sum of "cubed digits"
        }

    val genItr = (n: Int) => Iterator.iterate(n)(sumOfCubes)

    val itr1 = genItr(num).take(10)
    val itr2 = Example.ex6(genItr(num)).take(10)

    itr1
      .zip(itr2)
      .drop(1)
      .takeWhile { case (ii, i) => ii != i }
      .toList
      .map(_._1)
  }

  // 3
  //
  // Build collatz sequence, given n,
  // c_0 = n
  // c_k+1 = c_k/2        if c_k is even
  //       = 3c_k + 1     if c_k is odd
  def xr3(n: Int): Iterator[Int] = {
    Iterator
      .iterate(n) { x =>
        if (x % 2 == 0) x / 2
        else 3 * x + 1
      }
      .takeWhile(_ != 1)
  }

  // 4
  //
  // computer list of Sets Z containing three member Z={a,b,c} where
  // a E A, b E B, c E C
  def xr4(a: Set[Int], b: Set[Int], c: Set[Int]): Set[Set[Int]] = {
    a.flatMap { x =>
      b.flatMap { y =>
        c.map(z => Set(x, y, z))
      }
    }
  }

  // 5
  //
  // same as 4 but for Set[Set[Int]]
  // def xr5(s: Set[Set[Int]]): Set[Set[Int]] = {
  //   s match {
  //     case Set()     => Set(Set()) // no element
  //     case Set(x)    => x // one element
  //     case Set(x, y) => CartesianProduct.first_op(x, y) // two elements
  //     case _ =>
  //       val (xy, rest) = s.splitAt(2)
  //       rest.foldLeft(xr5(xy)) { (prev, x) =>
  //         CartesianProduct.subseq_op(prev, x)
  //       }
  //   }
  // }

  val sets = Set(
    Set(1, 2, 3, 4),
    Set(11, 22, 33, 44),
    Set(111, 222, 333, 444),
    Set(1111, 2222, 3333, 4444)
  )
}
