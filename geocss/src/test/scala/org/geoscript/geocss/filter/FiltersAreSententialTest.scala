package org.geoscript.geocss.filter

import org.geoscript.support.interval.Interval
import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL.toFilter

import org.scalatest.FunSuite, org.scalatest.matchers.ShouldMatchers

class FiltersAreSententialTest extends FunSuite with ShouldMatchers {
  import FiltersAreSentential._

  test("Null") {
    constraint(toFilter("A IS NULL")) should equal(IsNull("A"))
  }

  test("Not Null") {
    constraint(toFilter("A IS NOT NULL")) should equal(In("A", Interval.Full))
  }

  test("Not Equals") {
    constraint(toFilter("A <> 1")) should equal(IsNot("A", Value("1")))
  }

  test("Disproves") {
    disprovenBy(Set(toFilter("A <> 1")), toFilter("A IS NULL")) should equal(true)
  }
}
