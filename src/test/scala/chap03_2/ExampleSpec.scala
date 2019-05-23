package chap03_2

import org.scalatest._
import Example._

class ExampleSpec extends FlatSpec with Matchers {

  "Example 3-2.2" should "create disjoint type DayOfWeek" in {
    isSaturday(Saturday) shouldEqual true
    isSaturday(Monday) shouldEqual false
  }

  "Example 3-2.3" should "return None if either of the conjuncts are None otherwise return conjunction in Option" in {
    check((Some(1), Some("a"))) shouldEqual Some((1, "a"))
    check((None, Some("a"))) shouldEqual None
    check((Some("a"), None)) shouldEqual None
    check((None, None)) shouldEqual None
  }

}
