package libs

object CartesianProduct {

  // Cartesian product is binary operation
  // with associative property hence cartesian
  // product of any number of sets is possible to
  // compute (non performantly)
  def first_op[A](a: Set[A], b: Set[A]) = {
    a.flatMap(x => b.map(y => Set(x, y)))
  }

  // Will converge the top cases when I'll know generics
  // and typeclasses
  def subseq_op[A](a: Set[Set[A]], b: Set[A]) = {
    val set: Set[Set[A]] = Set()

    b.foldLeft(set) { (prev, x) =>
      a.flatMap(y => prev + (y + x))
    }
  }

}
