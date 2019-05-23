package chap03_3
import chap03.FunctionType._
import scala.annotation.tailrec

object Example {

  // 1
  //
  // Define a parameterize type MyT[T] for the short
  // type notation:
  // Boolean => (1 + T + Int x T + (String => T))
  type MyT[T] = Boolean => MyTVals[T]

  sealed trait MyTVals[T]
  final case class EmptyVal[T]() extends MyTVals[T]
  final case class SingleT[T](t: T) extends MyTVals[T]
  final case class IntWithT[T](i: Int, t: T) extends MyTVals[T]
  final case class StringToT[T](fn: String => T) extends MyTVals[T]

  // 2
  //
  // Transform (Either[A, B], Either[C, D]) to equivalent sum type.
  // (a + b) x (c + d) = ac + ad + bc + bd
  type Type1[A, B, C, D] = (Either[A, B], Either[C, D])

  sealed trait Type2[A, B, C, D]
  final case class T2AC[A, B, C, D](a: A, c: C) extends Type2[A, B, C, D]
  final case class T2AD[A, B, C, D](a: A, d: D) extends Type2[A, B, C, D]
  final case class T2BC[A, B, C, D](b: B, c: C) extends Type2[A, B, C, D]
  final case class T2BD[A, B, C, D](b: B, d: D) extends Type2[A, B, C, D]

  // Isomorphism test of Type1 and Type2
  def t1tot2[A, B, C, D]: Type1[A, B, C, D] => Type2[A, B, C, D] = {
    case (Left(a), Left(c))   => T2AC(a, c)
    case (Left(a), Right(d))  => T2AD(a, d)
    case (Right(b), Left(c))  => T2BC(b, c)
    case (Right(b), Right(d)) => T2BD(b, d)
  }

  def t2tot1[A, B, C, D]: Type2[A, B, C, D] => Type1[A, B, C, D] = {
    case T2AC(a, b) => (Left(a), Left(b))
    case T2AD(a, d) => (Left(a), Right(d))
    case T2BC(b, c) => (Right(b), Left(c))
    case T2BD(b, d) => (Right(b), Right(d))
  }
  // isomorphism is quite evident if you flip arguments and return
  // you get the other, flip arguments and return of t2tot1 and you
  // get t1tot2 and similarly t1tot2 flips to t2tot1

  //
  // ─── ILLOGICAL STATEMENTS LEAD TO NON-IMPLEMENTABILITY ──────────────────────────
  //
  //    (a x b) -> c = (a -> c) + (b -> c)
  // logical
  // ~(a x b) + c = (~a + c) + (~b + c)
  // ~a v ~b + c = ~a + ~b + c
  // + = exclusive or
  // v = inclusive or

  type Reader[E, T] = E => T

  def pure[E, A]: A => Reader[E, A] =
    a => _ => a

  // given a function from E => A return a function form E => B
  // E => A compose E => B = E => B
  def map[A, B, E]: (A => B) => Reader[E, A] => Reader[E, B] =
    fab => fea => compose(fea, fab)

  // 3-a
  // a + a = a (in logic) but not in types
  //
  // showing types `a + a` and `a` are not isomorphic
  // a + a -> a
  def sumToPrimitive1[A]: Either[A, A] => A = {
    case Left(a)  => a // losing info about position of `a`
    case Right(a) => a
  }
  // a -> a + a
  def sumToPrimitive2[A]: A => Either[A, A] = a => Left(a)

  // 3-b
  // a x a = a (in logic) but not in types
  def productToPrimitive1[A]
      : ((A, A)) => A = { case (a1, _) => a1 } // loss of info
  def productToPrimitive2[A]: A => (A, A) = a => (a, a)

  // 4
  // a x b => c /= (a => c) + (b => c)
  //
  // given a function from a x b => c
  // produce either of two functions
  // 1. a => c
  // 2. b => c
  /*
  def ex4a[A, B, C]: ((A, B) => C) => Either[(A => C), (B => C)] =
    fn => { cannot produce C because that require (a x b) and
            we have either a or b (a + b) }
   */
  //
  // (a => c) + (b => c) -> (a x b) => c is implementable
  def ex4[A, B, C]: Either[A => C, B => C] => (A, B) => C = {
    case Left(ac)  => { case (a, _) => ac(a) } // loss of info happens here
    case Right(bc) => { case (_, b) => bc(b) } // and here
  }

  // map : (a => b) => 1 + a => 1 + b
  // this function can be a trivial function that always returns
  // unit `1` which is always available.
  // no information loss is one of the criteria of `map` and it is
  // enforced with this property.
  // map id a = a (where a is an option type)
  def mapO[A, B]: (A => B) => Option[A] => Option[B] =
    fn => {
      case None    => None
      case Some(a) => Some(fn(a))
    }

  def mapE[L, R, B]: (R => B) => Either[L, R] => Either[L, B] =
    fn => {
      case Right(r) => Right(fn(r))
      case Left(l)  => Left(l)
    }

  def flatMapE[L, R, B]: Either[L, R] => (R => Either[L, B]) => Either[L, B] = {
    case Right(r) => fn => fn(r)
    case Left(l)  => _ => Left(l)
  }

  type State[S, T] = S => (T, S)

  def pures[S, A]: A => State[S, A] =
    a => s => (a, s)

  // A => B, S => (A, S) ==> (S => (B, S))
  def maps[S, A, B]: (A => B) => State[S, A] => State[S, B] =
    ab =>
      sas =>
        sas(_) match {
          case (a, s) => (ab(a), s)
        }

  // s => (a x s), a => (s => (b x s)), ? S => (b x s)
  def flatMaps[S, A, B]: State[S, A] => (A => State[S, B]) => State[S, B] =
    sas =>
      asbs =>
        s => {
          val (a, ns) = sas(s)
          val nsb = asbs(a)
          nsb(ns)
        }

  sealed trait NEList[A]
  final case class NEHead[A](head: A) extends NEList[A]
  final case class NETail[A](head: A, tail: NEList[A]) extends NEList[A]

  def mapNE[A, B]: (A => B) => NEList[A] => NEList[B] =
    fn => {
      case (NEHead(h))    => NEHead(fn(h))
      case (NETail(h, t)) => NETail(head = fn(h), tail = mapNE(fn)(t))
    }

  def mapNERec[A, B]: (A => B) => NEList[A] => NEList[B] =
    fn =>
      nel => {
        @tailrec
        def mapt(orig: NEList[A], accum: NEList[B]): NEList[B] = {
          orig match {
            case NEHead(head)       => NETail(fn(head), accum)
            case NETail(head, tail) => mapt(tail, NETail(fn(head), accum))
          }
        }

        reverseNE(nel) match {
          case NEHead(head)       => NEHead(fn(head))
          case NETail(head, tail) => mapt(tail, NEHead(fn(head)))
        }
      }

  def reverseNE[T](list: NEList[T]): NEList[T] = {

    @tailrec
    def reverseRec(xs: NEList[T], accum: NEList[T]): NEList[T] = xs match {
      case NEHead(head)       => NEHead(head)
      case NETail(head, tail) => reverseRec(tail, concatNE(accum)(NEHead(head)))
    }

    list match {
      case NETail(head, tail) => reverseRec(tail, NEHead(head))
      case _                  => list
    }
  }

  def concatNE[T]: NEList[T] => NEList[T] => NEList[T] =
    nea =>
      neb =>
        (nea, neb) match {
          case (NEHead(a), NEHead(b)) =>
            NETail(a, NEHead(b)) // doing same thing as below
          case (NEHead(a), NETail(h, t)) =>
            NETail(a, NETail(h, t)) // doing the same thing as above
          case (NETail(h, t), b) => NETail(h, concatNE(t)(b))

        }
}
