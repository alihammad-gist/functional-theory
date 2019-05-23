package chap03_3

import org.scalatest._
import Example._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.prop.{
  GeneratorDrivenPropertyChecks,
  TableDrivenPropertyChecks
}
import org.scalacheck.ScalacheckShapeless._
import chap03.FunctionType._
import org.scalatest.prop.Checkers
import org.scalacheck.Gen

class ExampleSpec extends FlatSpec with Matchers with Checkers {

  "Example 1" should "have type MyTVals[T]" in {
    val a: MyT[Int] = b => if (b) IntWithT(5, 5) else StringToT(s => s.toInt)
  }

  "Example 2" should "prove Equivelance of (a + b) x (c + d) = ac + ad + bc + bd" in {

    def check1[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]() = {
      val p1 = forAll { (t1: Type1[A, B, C, D]) =>
        t2tot1(t1tot2(t1)) == t1
      }

      val p2 = forAll { (t2: Type2[A, B, C, D]) =>
        t1tot2(t2tot1(t2)) == t2
      }

      check(p1 && p2);

    }

    check1[Int, Boolean, Double, List[Int]]()
  }

  "Example 3" should "prove a + a /= a" in {

    val p1 = forAll { (x: Int) =>
      compose(
        sumToPrimitive2[Int],
        sumToPrimitive1[Int]
      )(x) == x
    }

    val leftGen: Gen[Either[Int, Int]] = for {
      n <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield Right(n) // loss of information when Right is used

    val p2 = forAll(leftGen) { (x: Either[Int, Int]) =>
      compose(
        sumToPrimitive1[Int],
        sumToPrimitive2[Int]
      )(x) != x
    }

    check(p1 && p2);
  }

  "Example 3-b" should "prove a x a /= a" in {

    val unequalParts: Gen[(Int, Int)] = for {
      n <- Gen.choose(Int.MinValue, Int.MaxValue - 1)
      m <- Gen.const(n + 1)
    } yield (n, m)

    val equalParts: Gen[(Int, Int)] = for {
      n <- Gen.choose(Int.MinValue, Int.MaxValue - 1)
    } yield (n, n)

    val p1 = forAll(unequalParts) { (x: (Int, Int)) =>
      productToPrimitive2(productToPrimitive1(x)) != x
    }

    val p1b = forAll(equalParts) { (x: (Int, Int)) =>
      productToPrimitive2(productToPrimitive1(x)) == x
    }

    val p2 = forAll { (x: Int) =>
      productToPrimitive1(productToPrimitive2(x)) == x
    }

    check(p1 && p1b && p2);
  }

  "Map option" should "prove identity property" in {
    val p = forAll { (x: Int) =>
      mapO((a: Int) => a)(Option.apply(x)) == Option.apply(x)
    }

    check(p);
  }

  "Map Either" should "prove identity property" in {
    val p = forAll { x: Int =>
      mapE((a: Int) => a)(Right(x)) == Right(x) &&
      mapE((a: Int) => a)(Left(x)) == Left(x)
    }

    check(p)
  }

  "FlatMap Either" should "prove identity property" in {
    val p = forAll { x: Int =>
      flatMapE(Right(x))((a: Int) => Right(a)) == Right(x)
    }

    check(p)
  }

  "Map NEList" should "map function over items properly" in {
    mapNE((x: Int) => x + 1)(NETail(1, NETail(2, NETail(4, NEHead(5))))) shouldEqual
      NETail(2, NETail(3, NETail(5, NEHead(6))))
  }
}
