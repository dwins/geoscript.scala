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
    andA.asFilter must be (org.opengis.filter.Filter.INCLUDE)
    andB.asFilter must be (expr1.asFilter)
    andC.asFilter must be (
      filters.and(expr1.asFilter, expr2.asFilter)
    )

    // ((NotSelector(AcceptFilter))).asFilter must be (org.opengis.filter.Filter.EXCLUDE)
  }

  @Test def nots = {
    AcceptSelector.asFilter must be (org.opengis.filter.Filter.INCLUDE)
    NotSelector(AcceptSelector).asFilter must be (org.opengis.filter.Filter.EXCLUDE)
    NotSelector(NotSelector(AcceptSelector)).asFilter must be (org.opengis.filter.Filter.INCLUDE)
  }

  @Test def nested = {
    import org.opengis.filter.{PropertyIsGreaterThan,Not}
    val expr = ExpressionSelector("a>b")
    expr.asFilter must have (parent(classOf[PropertyIsGreaterThan]))
    val not = NotSelector(expr)
    not.asFilter must have (parent(classOf[Not]))
    val or = OrSelector(List(expr))
    or.asFilter must have (parent(classOf[PropertyIsGreaterThan]))
    val nor = OrSelector(List(not))
    nor.asFilter must have (parent(classOf[Not]))
    val and = AndSelector(List(expr))
    and.asFilter must have (parent(classOf[PropertyIsGreaterThan]))
    val nand = AndSelector(List(not))
    nand.asFilter must have (parent(classOf[Not]))
    val or_and = OrSelector(List(and))
    or_and.asFilter must have (parent(classOf[PropertyIsGreaterThan]))
    val or_nand = OrSelector(List(nand))
    or_nand.asFilter must have (parent(classOf[Not]))
  }
}
