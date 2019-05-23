package example

import org.scalatest._
import chap02.Exercise._

class ExerciseSpec extends FlatSpec with Matchers {

  "Exercise 3.1" should "sum squared digits of a decimal number" in {
    xr1(123) shouldEqual 14
  }

  "Exercise 3.2" should "return sequence of cubed digits" in {
    xr2(123) shouldEqual List(36, 243, 99, 1458, 702, 351, 153, 153, 153)
  }

  "Exercise 3.3" should "compute Collaz sequence" in {
    xr3(2223546).length shouldEqual 179
  }

  "Exercise 3.4" should "compute Set(x, y, z) where x E A, y E B, z E C" in {
    xr4(
      Set(1, 2),
      Set(11, 22),
      Set(111, 222)
    ) shouldEqual
      Set(
        Set(1, 11, 111),
        Set(1, 11, 222),
        Set(1, 22, 111),
        Set(1, 22, 222),
        Set(2, 11, 111),
        Set(2, 11, 222),
        Set(2, 22, 111),
        Set(2, 22, 222)
      )
  }
}
