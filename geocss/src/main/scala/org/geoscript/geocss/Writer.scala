package org.geoscript.geocss

final class Writer[A, B](val pair: (A, B)) extends AnyVal {
  def map[C](f: B => C): (A,C) = (pair._1, f(pair._2))
  def flatMap[C](f: B => (A, C))(implicit ma: Monoid[A]): (A,C) = {
    val (a0, c) = f(pair._2)
    val a1 = ma.mappend(pair._1, a0)
    (a1, c)
  }
}

object Writer {
  import language.implicitConversions
  implicit def sugar[A,B](pair: (A,B)): Writer[A,B] = new Writer(pair)
  def sequence[A : Monoid, B](seq: Seq[(A,B)]): (A, Seq[B]) = {
    val zero = implicitly[Monoid[A]].mzero
    seq.foldLeft((zero, Vector.empty[B])) {
      (acc, me) => for (a <- acc; e <- me) yield a :+ e
    }
  }
}
