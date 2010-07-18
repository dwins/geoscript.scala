package org.geoscript.geocss

import org.specs._
import org.opengis.filter.Filter.{ INCLUDE, EXCLUDE }

/**
 * Tests for token methods
 */
class TokenTest extends Specification {
  "'And' Selectors should do some basic simplification" in { 
    val filters =
      org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)
    val expr1 = ExpressionSelector("a>b")
    val expr2 = ExpressionSelector("c<d")

    val andA = AndSelector(Nil)
    val andB = AndSelector(List(expr1))
    val andC = AndSelector(List(expr1, expr2))
    andA.filterOpt must beSome(INCLUDE)
    andB.filterOpt must beSome(expr1.filterOpt.get)
    andC.filterOpt must beSome(
      filters.and(expr1.filterOpt.get, expr2.filterOpt.get)
    )
  }

  "'Not' selectors produce negated Filters" in {
    AcceptSelector.filterOpt must beSome(INCLUDE)
    NotSelector(AcceptSelector).filterOpt must beSome(EXCLUDE)
    NotSelector(NotSelector(AcceptSelector)).filterOpt must beSome(INCLUDE)
  }

  "Spot checks on miscellaneous filter combos" in {
    import org.opengis.filter.{PropertyIsGreaterThan,Not}
    val expr = ExpressionSelector("a>b")
    expr.filterOpt.get must haveSuperClass[PropertyIsGreaterThan]
    val not = NotSelector(expr)
    not.filterOpt.get must haveSuperClass[Not]
    val or = OrSelector(List(expr))
    or.filterOpt.get must haveSuperClass[PropertyIsGreaterThan]
    val nor = OrSelector(List(not))
    nor.filterOpt.get must haveSuperClass[Not]
    val and = AndSelector(List(expr))
    and.filterOpt.get must haveSuperClass[PropertyIsGreaterThan]
    val nand = AndSelector(List(not))
    nand.filterOpt.get must haveSuperClass[Not]
    val or_and = OrSelector(List(and))
    or_and.filterOpt.get must haveSuperClass[PropertyIsGreaterThan]
    val or_nand = OrSelector(List(nand))
    or_nand.filterOpt.get must haveSuperClass[Not]
  }
}
