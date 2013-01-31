package org.geoscript.geocss

import org.opengis.filter.Filter.{ INCLUDE, EXCLUDE }
import org.opengis.{ filter => ogc }
import org.scalatest._, matchers._

/**
 * Tests for token methods
 */
class TokenTest extends FunSuite with ShouldMatchers { 
  val filterFactory =
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)
  import filterFactory._

  val expr = (x: String) => 
    Selector.asSelector(org.geotools.filter.text.ecql.ECQL.toFilter(x))
  val expr1 = expr("a>b")
  val expr2 = expr("c<d")

  test("empty And goes to Include") {
    And(Nil).filterOpt should be(Some(INCLUDE))
  }

  test("single-element And goes to simple filter") {
    And(List(expr1)).filterOpt should equal(expr1.filterOpt)
  }

  test("'and' filter is correctly generated for larger And's") {
    val expected = 
      for { 
        f <- expr1.filterOpt
        g <- expr2.filterOpt
      } yield and(f, g)

    expectResult(expected) {
      And(List(expr1, expr2)).filterOpt
    }
  }

  test("'Not' selectors produce negated Filters") {
    Not(Accept).filterOpt should equal(Some(EXCLUDE))
    Not(Not(Accept)).filterOpt should equal(Some(INCLUDE))
    Accept.filterOpt should equal(Some(INCLUDE))
  }

  test("Spot checks on miscellaneous filter combos") {
    assert(expr1.filterOpt exists(_.isInstanceOf[ogc.PropertyIsGreaterThan]))
    assert(Not(expr1).filterOpt exists(_.isInstanceOf[ogc.Not]))
    assert(Or(List(expr1)).filterOpt exists(_.isInstanceOf[ogc.PropertyIsGreaterThan]))
    assert(And(List(expr1)).filterOpt exists(_.isInstanceOf[ogc.PropertyIsGreaterThan]))
    assert(Not(And(List(expr1))).filterOpt exists(_.isInstanceOf[ogc.Not]))
    assert(Or(List(And(List(expr1)))).filterOpt exists(_.isInstanceOf[ogc.PropertyIsGreaterThan]))
    assert(Or(List(And(List(Not(expr1))))).filterOpt exists(_.isInstanceOf[ogc.Not]))
  }
}
