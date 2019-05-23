package example

import org.scalatest._

class Chap02Spec extends FlatSpec with Matchers {
  "Example 1" should "return paired sequence of (cos(ai), sin(ai))" in {
    Chap02.ex1(Seq(3, 2, 1)) shouldEqual
      Seq(
        (Math.cos(3), Math.sin(3)),
        (Math.cos(2), Math.sin(2)),
        (Math.cos(1), Math.sin(1))
      )
  }

  "Example 2" should "return number of items where cos(ai) > sin(ai) in sequence a" in {
    Chap02.ex2(Seq(5, 4, 3, 2, 1)) shouldEqual 2 // cos(4), cos(5) are bigger
  }

  "Example 3" should "compute sequence c where ci = ai - bi for sequences a and b" in {
    Chap02.ex3(Seq(6, 5, 4), Seq(3, 2, 1)) shouldEqual Seq(3, 3, 3)
  }

  "Example 4" should "count how many times ai > a(i+1) for sequence a" in {
    Chap02.ex4(Seq(1, 2, 3, 4)) shouldEqual 0
    Chap02.ex4(Seq(4, 3, 2, 1)) shouldEqual 3
  }

  "Example 5" should "return list of max in a list of sliding windows 2k+1 wide; k > 0" in {
    Chap02.ex5(Seq(1, 2, 3, 4, 5, 6, 7), 1) shouldEqual Seq(3, 4, 5, 6, 7)
  }

  "Example 6" should "create a times table inside a map (Int, Int) -> Int" in {
    Chap02.ex6(3) shouldEqual
      Map(
        (1, 1) -> 1,
        (1, 2) -> 2,
        (1, 3) -> 3,
        (2, 1) -> 2,
        (2, 2) -> 4,
        (2, 3) -> 6,
        (3, 1) -> 3,
        (3, 2) -> 6,
        (3, 3) -> 9
      )
  }

  "Example 8" should "swap the indices and values of a map" in {
    val arg = Map(
      "user1" -> "address1",
      "user2" -> "address2",
      "user3" -> "address3"
    )
    Chap02.ex8(arg) shouldEqual
      Map(
        "address1" -> "user1",
        "address2" -> "user2",
        "address3" -> "user3"
      )
  }

}
