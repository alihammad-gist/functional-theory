package chap03_2

object Disjunctive {

  // # Conjuctions (Logical AND)

  // pair of values
  val a: (Int, String) = (234, "xyz")

  // giving meaning to a pair (alias)
  type MyPair = (Int, String)

  // MyPair allows *all* tuples of (Int, String)
  // and is not constraining enough
  val b: MyPair = (123, "xyz")

  // case class - tuple with names
  // MyPair allows all tuples typed (Int, String)
  // MySocks only allow MySocks
  case class MySocks(size: Double, color: String)
  val c: MySocks = MySocks(10.2, "white")

  // nested case classes
  case class BagOfSocks(socks: MySocks, count: Int)
  val bag = BagOfSocks(MySocks(10.2, "grey"), 6) // infered type is BagOfSocks

  // dot accessors for case classes
  val d = bag.socks.color

  // components can be given in any order by using names
  val e = MySocks(color = "black", size = 22.2)

  // default values for case classes
  case class Shirt(color: String = "blue", hasHoles: Boolean = false)
  val sock = Shirt(hasHoles = true) // other params inherit default values

  // (A, B) is syntactic suger for Tuple2[A, B]
  // case class Tuple2[A, B](_1: A, _2: B)
  val f: Tuple2[Int, String] = (123, "str") // 2 elements
  // val g: Tuple1[Int] = (23,) // one element
  val h: Unit = () // zero element

  // Case classes alternatives of above
  case class T2(x: Int, y: String) // two elements
  case class T1(x: Int) // one element
  case object C // empty tuple

  //================================================
  //      PATTERN MATCHING
  //================================================
  //
  // pattern matching works in two places
  // 1. `val pattern = ...`     (value assignment)
  // 2. `case pattern => ...`   (partial function)

  // 1. `val pattern = ...`
  val i = MySocks(10.5, "white")
  val MySocks(xx, yy) = i
  assert(xx == 10.5 && yy == "white")

  // 2. `case pattern => ...`  -- partial function
  val fn: BagOfSocks => String = {
    case BagOfSocks(MySocks(s, c), z) =>
      s.toString() + c + z.toString()
  }

  def fnd(b: BagOfSocks): String = b match {
    case BagOfSocks(MySocks(s, c), z) =>
      s.toString() + c + z.toString()
  }

  // # Disjunction (Logical OR)
  //
  // 1. The roots of quadratic equation can be either a pair, one or none
  //          (Real, Real) or Real or ()
  // 2. Binary search either finds the needle and its index or not
  //          (Int, Int) or ()
  // 3. A computation that either gives value or encounters an error
  //          Int or String
  // 4. Computer game states: Several kinds of room, player-types with distinct properties.
  //            case class 1 or case class 2 or ... or case class n
  //
  //
  // ## Mathematical point of view
  //
  //
  // f(x) where x E R implies R is the doman of f(x)
  //
  // -> We would like to represet arbitary *Disjoint* domains, for example x
  //    is either point on a line OR point on a surface
  //
  // -> In functional programing, disjoint domains are always *labeled* because
  //    distinction b/w different parts of the disjoint union is made by their labels.
  //        eg. x E (left, R)   U    (right, R^2)
  //              R point on            R^2 point on
  //                line                Surface
  //
  // -> Disjoint union is ALWAYS EXCLUSIVE-OR (either one of the options, not more than one)
  // -> Given any such x, we can determine its **side** of the union
  //    by using its label
  // -> Such Union types are denoted by Either in Scala, so for type of `x` we have
  type Xtype = Either[Double, (Double, Double)]

  // Pattern matching is used for dealing with disjunctive types
  def logErr(x: Either[String, Int]): Int = x match {
    case Left(err)  => println(err); -1
    case Right(res) => res
  }

  // Case expressions like about can let you treat any arbitary type
  // as disjunctive type
  def disjunctInt(x: Int): Int = x match {
    case 0 => 0
    case 1 => 1
    case _ => x * x
  }

  //==================================================================
  //              TRAITS (General Disjunction)
  //==================================================================
  //
  // traits are disjoint union of case classes (case classes represent domains)
  //
  // type Diss = List[Int] | (Int, Boolean) | MySocks
  sealed trait Diss
  final case class ListInt(x: List[Int]) extends Diss
  final case class IntBool(x: Int, b: Boolean) extends Diss
  final case class Socks(socks: MySocks) extends Diss
  //
  // `sealed` keyword restricts only those classes which are
  // present in the same file to extend the trait - the reason is that
  // if trait is extended further any case expression that deal with these
  // three domains will be incomplete and produce errors.
  // `final` keyword also prevents other case classes to extend these case classes
  // I assume so no other case class can pretend to be these.
  //
  // constructing Diss type
  val j: Diss = if (true) ListInt(List(1, 2, 3)) else IntBool(3, false)
  //
  // working with it
  def dissed(d: Diss): Int = d match {
    case ListInt(x) =>
      x.foldLeft[Int](0) { (prev, x) =>
        prev + x
      }
    case IntBool(x, _)        => x + 1
    case Socks(MySocks(s, _)) => s.toInt
  }

  // Option is another popular disjunct type known as Maybe in purescript
  def saveDivide(x: Double, y: Double): Option[Double] = y match {
    case 0 => None
    case _ => Some(x / y)
  }

}
