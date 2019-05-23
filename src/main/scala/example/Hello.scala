package example

object Hello extends Greeting with App {
  val factorial10 = (1 to 10).product

  def factorial(n: Int): Int = {
    (1 to n).product
  }

  def prime(n: Int): Boolean = {
    // forall i. 2 <= i <= n ; n /= 0 mode i
    (2 until n).forall(i => n % i != 0)
  }

  def count_even(n: Set[Int]): Int = {

    def is_even(k: Int): Int = if (k % 2 == 0) 1 else 0

    n.toSeq.map(is_even).sum
  }

  val f1 = (x: Int, y: Int) => { x * x + y * y }

  val add20: Int => Int = (x) => x + 20

  val make_func: Int => Int => Int = a => b => a + b

  println(factorial10)
  println(factorial(10))
  println(prime(12))
  println(prime(23))
  println(
    count_even(Set(1, 2, 3, 4, 5, 6))
  )
  println(f1(1, 1))
  println(f1(2, 2))
  println(f1(9, 9))
  println(add20(20))

  val fn50 = make_func(50)
  val ans = fn50(50)
  println(ans)

  val is_prime = (n: Int) => { (2 until n).forall(i => n % i != 0) }
  println(is_prime(3))
  println(is_prime(31))
  println(is_prime(32))

  // compute avg of all nums in a sequence of type double
  val average = (s: Seq[Double]) => s.sum / s.size
  println(average(Seq(1, 2, 3, 4)))

  val willisProduct = (n: Int) => {
    (1 to n)
      .map(i => i.toDouble)
      .map(j => {
        (4.0 * j * j) / ((2.0 * j - 1) * (2.0 * j + 1))
      })
        .product
  }

  println(willisProduct(10000))
  println(math.cos(willisProduct(10000)))

  // sequences of sets filtered to give give sequence of sets with
  // set size equalling 3
  val seqSets = (s: Seq[Set[Int]]) => s.filter(i => i.size > 2)
  println(
    seqSets(
      Seq(
        Set(1, 2, 3, 4, 5),
        Set(1, 3, 4),
        Set(1, 4),
        Set(1)
      )
    )
  )

  // ex1
  val normalizer = (s: Seq[Double]) => {
    if (s.max > 0) s.map(i => i / s.max)
    else s
  }

  val innerSeq = (s: Seq[Seq[Int]]) => s.map(ss => ss.map(i => i + 20))

  def _3factors(): Set[Int] = {
    val s = (1 to 1000).toSet

    val is_3factor = (n: Int) => {
      (2 to (n / 2)).filter(i => n % i == 0).size == 3
    }

    s.filter(is_3factor)
  }
  println(_3factors())

  // composition
  val composeFn = (f: Int => Double, g: Double => String) => (a: Int) => g(f(a))


}

trait Greeting {
  lazy val greeting: String = "Hello World~"
}
