package org.geoscript.geocss

import org.geotools.filter.text.ecql.ECQL
import org.scalatest._, matchers._

class SelectorTest extends FunSuite with ShouldMatchers {
  import Selector.SelectorsAreSentential.{ disprovenBy, provenBy }

  def scale_<(s: String): Selector = PseudoSelector("scale", "<", s)
  def scale_>(s: String): Selector = PseudoSelector("scale", ">", s)
  def not(s: Selector): Selector = Not(s)
  val cql = (ECQL.toFilter(_: String)) andThen (Selector.asSelector)

  test("disproven test") {
    assert(disprovenBy(Set(scale_>("1")), scale_<("0")))
    assert(disprovenBy(Set(scale_>("1")), not(scale_>("0"))))
    assert(disprovenBy(Set(not(scale_>("0"))), scale_>("1")))
    assert(disprovenBy(Set(cql("A=1")), cql("A = 2")))

    assert(!disprovenBy(Set(scale_>("1")), scale_>("0")))
    assert(!disprovenBy(Set(cql("A=2")), cql("A = 2")))
  }

  test("proven test") {
    assert(!provenBy(Set(cql("A=1")), cql("A=2")))
    assert(provenBy(Set(cql("A=2")), cql("A=2")))
    assert(provenBy(Set(scale_>("1")), scale_>("0")))
  }
}
