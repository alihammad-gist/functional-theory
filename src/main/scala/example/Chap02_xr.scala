package example

object Chap02Exercises {

  // 1
  // Find all i, j within (0,1, ... ,9) such that i + 4j > ij
  // -> similary for i,j,k and the condition i + 4j + 9k > i.k.j
  def xr1 = (n: Int) => {

    val pred: ((Int, Int)) => Boolean = { tuple =>
      tuple match {
        case (i, j) => i + 4 * j > i * j
      }
    }

    (0 to n)
      .flatMap { i =>
        (0 to n).map { j =>
          (i, j)
        }
      }
      .filter(pred)
  }

  // same as 1, with triplet
  def xr1b = (n: Int) => {
    (0 to 9)
      .flatMap(
        i =>
          (0 to 9)
            .flatMap(
              j =>
                (0 to 9)
                  .map(k => (i, j, k))
            )
      )
      .filter { case (i, j, k) => i + 4 * j + 9 * k > i * j * k }
  }

  // 2
  // given two sequences a, b where a.length = b.length, computer sequece c where
  // cj = ai when bi = true
  def xr2 =
    (a: Seq[String], b: Seq[Boolean]) =>
      a.zip(b).filter { case (_, bi) => bi }.map { case (ai, _) => ai }

  // 3
  // convert sequence of Ints (a) to sequence of (Int, Boolean) (b) where boolean is true
  // when a(i) > a(i+1)
  def xr3 =
    (a: Seq[Int]) =>
      a.zip(a.tail).map {
        case (ai, ai1) =>
          if (ai < ai1) (ai, true)
          else (ai, false)
      }

  // 4
  // given two sequences a and b, compute a map with (ai -> bi)
  def xr4 =
    (a: Seq[String], b: Seq[Int]) => a.zip(b).toMap

  // 4b
  // generic version
  def xr4b[A, B] =
    (a: Seq[A], b: Seq[B]) => a.zip(b).toMap

  // 5
  // given sequences a and b where b is Ints, compute a sequence
  // that contains strings from a ordered by b.
  def xr5[S] =
    (a: Seq[S], b: Seq[Int]) =>
      a.zip(b).sortBy { case (_, ord) => ord }.map { case (s, _) => s }

  def xr2_1 =
    (a: Seq[(String, Int)]) =>
      a.groupBy { case (s, i) => s }.map {
        case (s, seq) => (s, seq.foldLeft(0) { case (acc, (_, n)) => acc + n })
      }

  def xr2_2 =
    (a: Seq[List[Int]]) => a.map(_.sorted(Ordering[Int].reverse).take(3))

  // Cartesian product
  def xr2_3[A, B] =
    (a: Set[A], b: Set[B]) => a.flatMap(x => b.map((x, _)))

  def xr2_4[Person, Amount] =
    (a: Seq[Map[Person, Amount]]) =>
      a.flatMap(m => m.toSeq)
        .groupBy {
          case (person, _) => person
        }
        .map { case (p, sq) => (p, sq.map { case (_, amount) => amount }) }
}
