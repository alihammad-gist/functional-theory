package chap02

import org.scalatest._
import chap02.Example._

class ExampleSpec extends FlatSpec with Matchers {

  "Example 1" should "compute smallest n such that f(f(f..f(1)..)) > 1000" in {
    ex1({ 2 * _ + 1 }, 1, 1000) shouldEqual 9
  }

  "Example 2" should "compute kth largest element from unsorted sequence" in {
    val (k, s) = (3, Seq(1, 2, 4, 1, 2, 5, 6, 78, 9, 2, 322, 2, 111))
    ex2(s, k) shouldEqual 78
  }

  "Example 3" should "find last element of non-empty sequence" in {
    ex3(Seq(1, 2, 3, 10)) shouldEqual 10
  }

  "Example 4" should "find a number using binary search on a sorted list" in {
    val a = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    ex4(a, 2) shouldEqual 2
    ex4(a, 10) shouldEqual 9
    ex4(a, -1) shouldEqual 1
  }

  "Example 5" should
    """
    computer the sequence s given an int n, 
    such s_0 = SD(n) and s_k = SD(s_(k-1) for k >1
    where, SD(p) is the sum of decimal digits of int p, 
    eg, SD(123) = 6
    """ in {
    ex5(5555).take(3).toList shouldEqual List(20, 2, 0)
  }

  "Example 6" should "compute half-speed sequence" in {
    ex6(List(1, 2, 3, 4).toIterator).toList shouldEqual List(1, 1, 2, 2, 3, 3,
      4, 4)
  }

  "Example 7" should "cut off a given sequence at a place k where an element s_k equals earlier element" in {

    ex07(List(1, 3, 5, 7, 3, 5, 7, 3, 5, 7, 3, 5, 7)) shouldEqual
      List(1, 3, 5, 7, 3)
  }
}
