package org.geoserver.community.css

import org.scalatest.matchers.{HavePropertyMatcher, HavePropertyMatchResult}

// Utilities and such for CSS tests.

/**
 * An extra matcher for making basic "is-a" assertions on the
 * classes of objects, without requiring strict equality.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
trait TypeMatcher {
 def parent[A](expected: Class[A]) =
   new HavePropertyMatcher[AnyRef, Class[_]] {
     def apply(a: AnyRef) =
       HavePropertyMatchResult(
       expected.isAssignableFrom(a.getClass),
       "superclass",
       expected,
       a.getClass
     )
   }
}
