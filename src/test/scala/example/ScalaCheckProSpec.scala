package example

import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop

class ScalaCheckProSpec extends Properties("ali is scalacheck prop checker") {

  property("property 1") = forAll { x: Int =>
    x < Integer.MAX_VALUE ==> x + 1 > x
  }

  property("NonEmpty List property") = forAll(
    Gen.nonEmptyListOf(arbitrary[Int]),
    Gen.nonEmptyListOf(arbitrary[Int])
  ) { (l1, l2) =>
    l1.length < (l1 ::: l2).length
  }

}
