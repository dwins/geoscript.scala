package org.geoserver.community.css

import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

/**
 * Tests for token methods
 */
class TokenTest extends JUnitSuite with MustMatchersForJUnit with TypeMatcher {
  @Test def ands = {
    val filters =
      org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)
    val expr1 = ExpressionSelector("a>b")
    val expr2 = ExpressionSelector("c<d")

    val andA = AndSelector(Nil)
    val andB = AndSelector(List(expr1))
    val andC = AndSelector(List(expr1, expr2))
    andA.filterOpt.get must be (org.opengis.filter.Filter.INCLUDE)
    andB.filterOpt.get must be (expr1.filterOpt.get)
    andC.filterOpt.get must be (
      filters.and(expr1.filterOpt.get, expr2.filterOpt.get)
    )

    // ((NotSelector(AcceptFilter))).filterOpt.get must be (org.opengis.filter.Filter.EXCLUDE)
  }

  @Test def nots = {
    AcceptSelector.filterOpt.get must be (org.opengis.filter.Filter.INCLUDE)
    NotSelector(AcceptSelector).filterOpt.get must be (org.opengis.filter.Filter.EXCLUDE)
    NotSelector(NotSelector(AcceptSelector)).filterOpt.get must be (org.opengis.filter.Filter.INCLUDE)
  }

  @Test def nested = {
    import org.opengis.filter.{PropertyIsGreaterThan,Not}
    val expr = ExpressionSelector("a>b")
    expr.filterOpt.get must have (parent(classOf[PropertyIsGreaterThan]))
    val not = NotSelector(expr)
    not.filterOpt.get must have (parent(classOf[Not]))
    val or = OrSelector(List(expr))
    or.filterOpt.get must have (parent(classOf[PropertyIsGreaterThan]))
    val nor = OrSelector(List(not))
    nor.filterOpt.get must have (parent(classOf[Not]))
    val and = AndSelector(List(expr))
    and.filterOpt.get must have (parent(classOf[PropertyIsGreaterThan]))
    val nand = AndSelector(List(not))
    nand.filterOpt.get must have (parent(classOf[Not]))
    val or_and = OrSelector(List(and))
    or_and.filterOpt.get must have (parent(classOf[PropertyIsGreaterThan]))
    val or_nand = OrSelector(List(nand))
    or_nand.filterOpt.get must have (parent(classOf[Not]))
  }
}
