package example

object TCollection {

  val a = (1, 2, 3)

  def fn(x: (Int, Int, Int)): Int = {
    x match {
      case (x, y, z) => x + y + z
    }
  }

  val s = Seq(1, 2, 3)
  val t = Seq('a', 'b', 'c')

  val zips = s.zip(t)

  val m = zips.toMap

  val ans1 = m.map {
    case (x, y) =>
      if (y == 'b') x.toString()
      else "no"
  }.toList
}
