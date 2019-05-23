package chap03_3

import Example._

object Exercise {

  // 1
  //
  // Define type MyTU[T, U] for
  // 1 + t x u + Int x t + String x u
  sealed trait MyTU[T, U];
  final case class Unit[T, U]() extends MyTU[T, U];
  final case class TU[T, U](t: T, u: U) extends MyTU[T, U];
  final case class IntT[T, U](t: T, i: Int) extends MyTU[T, U];
  final case class StringU[T, U](u: U, s: String) extends MyTU[T, U];

  // 2
  //
  // show that a -> (b + c) /= (a -> b) + (a -> c)
  // arithematic: (b + c) ^ a = b ^ a + c ^ a
  // logic: ~a or (b xor c) /= (~a or b) xor (~a or c)
  def xr2[A, B, C]: (A => Either[B, C]) => Either[A => B, A => C] =
    fn => ???
  // couldn't decide which function to implement
  // don't have an `a` that can be applied to `a -> (b + c)`
  // return type of which will determine Left/Right.
  // if we get B then Left(A => B) otherwise Right(A=>C)

  // 3
  //
  // Transform Either[A, Int], Either[A, Char], Either[A, Float] to
  // shorthand
  // forall a. [(a + int), (a + char), (a + float)]

  // 4
  //
  // Define OpEither = forall a b. 1 + a + b
  sealed trait OpEither[+A, +B]
  final case object Unit extends OpEither[Nothing, Nothing]
  final case class Left[A, B](a: A) extends OpEither[A, B]
  final case class Right[A, B](b: B) extends OpEither[A, B]

  def mapOpE[A, B, C]: (B => C) => OpEither[A, B] => OpEither[A, C] =
    fbc => {
      case Unit     => Unit
      case Left(a)  => Left(a)
      case Right(b) => Right(fbc(b))
    }

  def flatMapOpE[A, B, C]
      : (B => OpEither[A, C]) => OpEither[A, B] => OpEither[A, C] =
    fbac => {
      case Unit     => Unit
      case Left(a)  => Left(a)
      case Right(b) => fbac(b)
    }

  def mapT[A, B]: (A => B) => MyT[A] => MyT[B] =
    fab =>
      fbola =>
        fbola(_) match {
          case EmptyVal()     => EmptyVal()
          case SingleT(a)     => SingleT(fab(a))
          case IntWithT(i, a) => IntWithT(i, fab(a))
          case StringToT(fn)  => StringToT((x: String) => fab(fn(x)))
        }

  def mapTU[A, T, U]: (U => A) => MyTU[T, U] => MyTU[T, A] =
    fua => {
      case TU(t, u)      => TU(t, fua(u))
      case StringU(u, s) => StringU(fua(u), s)
      case IntT(t, i)    => IntT(t, i)
      case Unit()        => Unit()
    }

  type State[S, A] = S => (A, S)

  def ex6a[A, B, S]: State[S, A] => ((S, A) => (S, B)) => State[S, B] =
    sas =>
      sasb =>
        sas(_) match {
          case (a, s) =>
            sasb(s, a) match {
              case (s, b) => (b, s)
            }
        }

  def ex6b[A, B, Z]: Either[A, Z] => (A => B) => Either[B, Z] = {
    case scala.util.Left(a)  => ab => scala.util.Left(ab(a))
    case scala.util.Right(z) => _ => scala.util.Right(z)
  }

  def ex6c[A, B, C, Z]
      : Either[A, Z] => Either[B, Z] => (A => B => C) => Either[C, Z] =
    az =>
      bz =>
        abc =>
          (az, bz) match {
            case (scala.util.Left(a), scala.util.Left(b)) =>
              scala.util.Left(abc(a)(b))
            case (_, scala.util.Right(z)) => scala.util.Right(z)
            case (scala.util.Right(z), _) => scala.util.Right(z)
          }

  type Reader[E, T] = E => T

  def flatMapET[E, A, B]: Reader[E, A] => (A => Reader[E, B]) => Reader[E, B] =
    ea => aeb => e => aeb(ea(e))(e)

}
