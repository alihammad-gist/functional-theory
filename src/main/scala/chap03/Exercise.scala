package chap03

import FunctionType._

object Exercise {

  // practically same as id itself
  // id takes a value and returns it as is
  // this is function application hence we get
  // back the `id[Int]` we provided
  def id_ = id(id)

  // we get back const function
  def const_ = id(const)

  // congrats another copy of id function hoo bahoo
  def id_1[T] = id(id[T => T])(id)

  // yet another one
  def id_2[T] = id(id(id[T]))
  // similar to
  def id_3[T] = compose(compose(id[T], id[T]), id[T])

  def const_const[A, B] = const(const[A, B])

  def swapFunc[A, B]: ((A, A) => B) => ((A, A) => B) =
    f => (a: A, aa: A) => f(aa, a)

}
