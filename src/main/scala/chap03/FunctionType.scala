package chap03

object FunctionType {

  // Currying
  def logWith(topic: String): (String => Unit) = { x =>
    println(s"$topic: $x")
  }

  val logWiths = (topic: String) =>
    (x: String) => {
      println(s"$topic  safsdf $x")
    }

  // parameteric polymorphoism

  // conjunctive commutitivity
  // a and b = b and a
  def swap[X, Y]: ((X, Y)) => (Y, X) = {
    case (x, y) => (y, x)
  }

  // hypothetical syllogism
  // a => b AND b => c ==> a => c
  def compose[X, Y, Z]: (X => Y, Y => Z) => (X => Z) =
    (f, g) => x => g(f(x))

  // identity
  // a ==> a
  def id[T]: T => T =
    x => x

  def const[C, X]: C => X => C =
    c => _ => c

}
