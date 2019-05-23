package chap03_2

object Example {

  sealed trait DayOfWeek
  final case object Sunday extends DayOfWeek
  final case object Monday extends DayOfWeek
  final case object Tuesday extends DayOfWeek
  final case object Wednesday extends DayOfWeek
  final case object Thursday extends DayOfWeek
  final case object Friday extends DayOfWeek
  final case object Saturday extends DayOfWeek

  val a: DayOfWeek = Monday
  val b = Monday // infered type won't be `DayOfWeek` disjoint type

  def isSaturday(d: DayOfWeek): Boolean = d match {
    case Saturday => true
    case _        => false
  }

  // option type
  def check[A, B]: ((Option[A], Option[B])) => Option[(A, B)] =
    x =>
      x match {
        case (Some(a), Some(b)) => Some((a, b))
        case _                  => None
      }
}
