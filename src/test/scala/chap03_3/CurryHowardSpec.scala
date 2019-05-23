package chap03_3

import org.scalatest._
import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop.{forAll, exists}
import org.scalatest.prop.{
  GeneratorDrivenPropertyChecks,
  TableDrivenPropertyChecks
}
import CurryHoward._

class CurryHowardSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with TableDrivenPropertyChecks {

  //
  // ─── TESTING ISOMORPHISMS ───────────────────────────────────────────────────────
  //
  // To test isomorphism ie.
  // Given
  // f: a -> b
  // g: b -> a
  //
  // id_a: a -> a
  // id_b: b -> b
  //
  // Type `a` is isomorphic to `b`. `a = b`
  //  => g . f = id_a
  //  AND
  //  => f . g = id_b

  "Identity 1" should "A x 1 = A" in {
    forAll { (proof: (Int, Unit)) =>
      a2(a1(proof)) shouldEqual proof //- g . f = id_a
    }
    forAll { (proof: Int) =>
      a1(a2(proof)) shouldEqual proof //- f . g = id_b
    }
  }

  "Associativity" should "prove (A x B) x C = A x (B x C)" in {
    forAll { (proof: ((Int, String), Boolean)) =>
      b2(b1(proof)) shouldEqual proof
    }

    forAll { (proof: (Int, (Boolean, Double))) =>
      b1(b2(proof)) shouldEqual proof
    }

  }

  "Conjunction distributes over disjunction" should "prove (a + b) x c == a x c + b x c" in {
    forAll { (proof: (Either[Int, Boolean], String)) =>
      c2(c1(proof)) shouldEqual proof
    }

    forAll { (proof: Either[(Int, String), (Double, String)]) =>
      c1(c2(proof))
    }
  }

  "Disjunction distributes over conjunction" should "prove (a x b) + c /= (a + c) x (b + c)" in {
    forAll { (proof: Either[(Int, String), Double]) =>
      d2(d1(proof)) shouldEqual proof //- g . f = id_a
    }

    forAll { (proof: (Either[Int, String], Either[Double, String])) =>
      whenever(proof match {
        case ((Left(_), Right(_))) => true
        case (_, _)                => false
      }) {
        //- f . g /= id_b (existential quantifier not universal)
        d1(d2(proof)) should not equal proof
      }
    }
  }

  "Definition of sum type" should "prove (a + b) -> c ==> a -> c + b -> c" in {
    forAll { (n: Either[Int, String], fn: Either[Int, String] => Boolean) =>
      e2(e1(fn))(n) shouldEqual fn(n)
    }

    forAll { (a: Int, b: Double, fna: Int => String, fnb: Double => String) =>
      e1(e2((fna, fnb))) match {
        case (fa, fb) =>
          fa(a) shouldEqual fna(a)
          fb(b) shouldEqual fnb(b)
      }
    }
  }
}
