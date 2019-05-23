package example

import org.scalatest._
import prop._
import matchers._
import org.scalactic._
import GeneratorDrivenPropertyChecks._
import anyvals._

class PropTestSpec extends FlatSpec with Matchers {

  "ensuring" should "should check asertion about the expression on left" in {
    42 ensuring (i => i > 41)
  }

  "whenever" should "should check when a condition is met" in {
    forAll { (x: Double) =>
      {
        whenever(x >= 0.0 && !x.isPosInfinity) {
          val r = math.sqrt(x)
          val t = math.ulp(x)
          r * r should ===(x +- t)
        }
      }
    }
  }

}
