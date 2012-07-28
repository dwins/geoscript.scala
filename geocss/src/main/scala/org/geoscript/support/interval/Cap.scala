package org.geoscript.support.interval

sealed trait Cap[T] {
  implicit def evidence: Ordering[T]
  def v: T
  def left: Interval[T] = Interval.left(this)
  def right: Interval[T] = Interval.right(this)
  def to(that: Cap[T]) = Interval.finite(this, that)
}

case class Open[T](v: T)(implicit val evidence: Ordering[T]) extends Cap[T]

case class Closed[T](v: T)(implicit val evidence: Ordering[T]) extends Cap[T]

object Cap {
  def satisfies[T](comp: (T, T) => Boolean)(c: Cap[T], u: T): Boolean =
    c match {
      case Closed(v) => v == u || comp(u, u)
      case Open(v) => comp(v, u)
    }

  def join[T](comp: (T, T) => Boolean)(a: Cap[T], b: Cap[T]): Cap[T] = 
    if (a.v == b.v)
      a match {
        case Open(_) => a
        case _ => b
      }
    else
      if (comp(a.v, b.v)) a else b
}
