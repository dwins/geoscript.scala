package org.geoscript.geocss

trait Monoid[T] {
  def mappend(a: T, b: T): T
  def mzero: T
}

object Monoid {
  implicit def listMonoid[T]: Monoid[List[T]] = 
    new Monoid[List[T]] {
      def mappend(a: List[T], b: List[T]): List[T] = a ::: b
      def mzero: List[T] = Nil
    }
}
