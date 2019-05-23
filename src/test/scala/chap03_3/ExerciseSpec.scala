package chap03_3

import org.scalatest._
import org.scalacheck.Gen
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.ScalacheckShapeless._
// import org.scalacheck.Arbitrary
import org.scalatest.prop.Checkers

import chap03.FunctionType._
import Exercise._
import Example._

class ExerciseSpec extends FlatSpec with Matchers with Checkers {

  "OpEither" should "operations should preserve information" in {

    // val leftGen = for {
    //   m <- Gen.choose(Int.MinValue, Int.MaxValue)
    // } yield Left(m)

    // val rightGen = for {
    //   m <- Gen.choose(Int.MinValue, Int.MaxValue)
    // } yield Right(m)

    val p1 = forAll { (x: OpEither[Int, Int]) =>
      mapOpE((a: Int) => a)(x) == x
    }

    val p2 = forAll { (x: OpEither[Int, Int]) =>
      flatMapOpE((a: Int) => x)(x) == x
    }

    check(
      p1 :| "id <$> x = x"
        && p2 :| "FlatMapOpE returns monad as is"
    );
  }

  "MyT Map" should "Preseve information (checked using id on map)" in {
    val p = forAll { (fn: MyT[Int], x: Boolean, y: String) =>
      (fn(x), mapT(id[Int])(fn)(x)) match {
        case (StringToT(fa), StringToT(fb)) => fa(y) == fb(y)
        case (a, b)                         => a == b
      }
    }

    check(p);
  }

  "MyTU Map" should "Preserve information (checked using id as function to map)" in {
    val p = forAll { (x: MyTU[Int, Double]) =>
      mapTU((a: Double) => a)(x) == x
    }

    check(p)
  }
}
