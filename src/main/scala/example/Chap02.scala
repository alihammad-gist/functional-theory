package example

object Chap02 {

  // 1
  // for a given sequence ai, compute the sequence of pairs bi = (cos ai, sin ai)
  // use .map assume a: Seq[Double]

  val ex1 = (a: Seq[Double]) => a.map(i => (math.cos(i), math.sin(i)))

  // 2
  // in a given sequence a, count how many times cos(ai) > sin(ai)
  val ex2 = (a: Seq[Double]) => a.count(i => math.cos(i) > math.sin(i))

  // 3
  // given a, b sequences, compute ci = ai - bi
  // use .map .zip
  val ex3 = (a: Seq[Double], b: Seq[Double]) =>
    a.zip(b).map { case (ai, bi) => ai - bi }

  // 4
  // given sequence a, count how many time a(i) > a(i+1) occurs
  val ex4 = (a: Seq[Double]) =>
    a.zip(a.tail).count { case (ai, ai1) => ai > ai1 }

  // 5
  // for a given k > 0, compute the sequence bi = max( a_(i-k), ... , a_(i+k) )
  val ex5 = (a: Seq[Double], k: Int) => a.sliding(2 * k + 1).map { _.max }.toSeq

  // 6
  // make times table in a map liek (1, 1) -> 1, ...
  val ex6 = (k: Int) =>
    (1 to k).map(i => (1 to k).map(j => ((i, j), i * j))).flatten.toMap

  // 8
  // invert indices and values of a Map[String, String]
  def ex8[A, B] = (m: Map[A, B]) => m.map(_.swap)
}
