package example

import org.scalatest._

class Chap02ExercisesSpec extends FlatSpec with Matchers {

  "Exercise 1.1" should "list (i, j) where i, j in (0 .. 9) and i+4j > i*j" in {
    Chap02Exercises.xr1(9).forall { case (i, j) => i + 4 * j > i * j } shouldBe true
  }

  "Exercise 1.1b" should "list (i, j, k) where i, j, k in (0 .. 9) and i+4j+9k > i*j*k" in {
    Chap02Exercises.xr1b(9).forall {
      case (i, j, k) => i + 4 * j + 9 * k > i * j * k
    } shouldEqual true
  }

  "Excersize 1.2" should "compute Seq[String] c, from sequences a, b where ci = aj when bj is true" in {
    val a = Seq("Ali", "Hammad", "Shah", "Naqvi")
    val b = Seq(true, false, false, true)

    Chap02Exercises.xr2(a, b) shouldEqual Seq("Ali", "Naqvi")
  }

  "Exercise 1.3" should "convert sequence a:int to sequence (a, bool) where bool is true when ai < a(i+1) " in {
    Chap02Exercises.xr3(Seq(1, 2, 3, 4, 4)) shouldEqual
      Seq(
        (1, true),
        (2, true),
        (3, true),
        (4, false)
      )
  }

  "Exercise 1.4" should "compute a Map(ai -> bi) where a, b are sequences" in {
    val a = Seq("u1", "u2", "u3")
    val b = Seq(21, 22, 22)

    Chap02Exercises.xr4(a, b) shouldEqual
      Map(
        "u1" -> 21,
        "u2" -> 22,
        "u3" -> 22
      )

    // generic version
    Chap02Exercises.xr4b(
      Seq((1, 2), (1, 3), (1, 1)),
      Seq(3, 4, 2)
    ) shouldEqual
      Map(
        (1, 1) -> 2,
        (1, 2) -> 3,
        (1, 3) -> 4
      )
  }

  "Exercise 1.5" should "computer string sequence c ordered int sequence b from string sequence a " in {
    Chap02Exercises.xr5(
      Seq("Hammad", "Shah", "Ali", "Naqvi"),
      Seq(2, 3, 1, 4)
    ) shouldEqual
      Seq("Ali", "Hammad", "Shah", "Naqvi")
  }

  "Exercise 2.1" should "add numbers in tuple (string, number) for a particular string" in {
    Chap02Exercises.xr2_1(
      Seq(
        ("apple", 5),
        ("apple", 5),
        ("apple", 5),
        ("orange", 10),
        ("orange", 10),
        ("mango", 1)
      )
    ) shouldEqual
      Map(
        "apple" -> 15,
        "orange" -> 20,
        "mango" -> 1
      )
  }

  "Exercise 2.2" should "computer a sequence of List contianing 3 sorted numbers from another sequence" in {
    Chap02Exercises.xr2_2(
      Seq(
        List(3, 4, 5, 2, 6, 78, 2, 1, 23),
        List(2020, 123, 54, 5, 6, 67, 56756, 723),
        List(5, 23, 456, 6, 7),
        List(23423, 234, 234, 234, 234, 566)
      )
    ) shouldEqual
      Seq(
        List(78, 23, 6),
        List(56756, 2020, 723),
        List(456, 23, 7),
        List(23423, 566, 234)
      )
  }

  "Exercise 2.3" should "compute cartesian product of two sets" in {
    Chap02Exercises.xr2_3(
      Set("a", "b", "c"),
      Set(true, false)
    ) shouldEqual
      Set(
        ("a", true),
        ("a", false),
        ("b", true),
        ("b", false),
        ("c", true),
        ("c", false)
      )
  }

  "Exercise 2.4" should "Compute expenditure per person from a sequence of expenditure records" in {
    Chap02Exercises.xr2_4(
      Seq(
        Map(
          "ali" -> 32,
          "waqas" -> 44,
          "ahmed" -> 99,
          "virk" -> 33
        ),
        Map(
          "waqas" -> 324,
          "ahmed" -> 994
        ),
        Map(
          "ali" -> 323,
          "waqas" -> 434,
          "virk" -> 32,
          "ahmed" -> 299
        )
      )
    ) shouldEqual
      Map(
        "ali" -> Seq(32, 323),
        "waqas" -> Seq(44, 324, 434),
        "ahmed" -> Seq(99, 994, 299),
        "virk" -> Seq(33, 32)
      )
  }
}
