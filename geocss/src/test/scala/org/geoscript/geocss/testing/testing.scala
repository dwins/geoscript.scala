package org.geoscript.geocss

import org.scalatest.matchers.{
  BeMatcher, Matcher, ShouldMatchers, MatchResult
}

package object testing extends ShouldMatchers {
  def closeTo(x: Double, eps: Double): BeMatcher[Double] =
    new BeMatcher[Double] {
      def apply(d: Double) = new MatchResult(
        math.abs(d - x) <= eps,
        "%f was not within %f of %f".format(d, x, eps),
        "%f was within %f of %f".format(d, x, eps))
    }

  def containAll[A](x: A, xs: A*): Matcher[Traversable[A]] = 
    (xs foldLeft (contain(x))) { (x, y) => x and contain(y) }
}
