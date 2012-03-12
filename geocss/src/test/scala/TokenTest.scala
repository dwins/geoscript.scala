package org.geoscript.geocss

import org.specs2._
import org.opengis.filter.Filter.{ INCLUDE, EXCLUDE }
import org.opengis.{ filter => ogc }

/**
 * Tests for token methods
 */
class TokenTest extends Specification with matcher.DataTables { 
  val filterFactory =
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)
  import filterFactory._

  val expr1 = ExpressionSelector("a>b")
  val expr2 = ExpressionSelector("c<d")

  def is = 
    "'And' Selectors should do some basic simplification" ^ 
       "empty And goes to Include" ! {
         AndSelector(Nil).filterOpt must beSome(INCLUDE)
       } ^ 
       "single-element And goes to simple filter" ! {
         AndSelector(List(expr1)).filterOpt must_== expr1.filterOpt
       } ^
       "'and' filter is correctly generated for larger And's" ! {
         AndSelector(List(expr1, expr2)).filterOpt must_== 
           (for { f <- expr1.filterOpt; g <- expr2.filterOpt }
             yield and(f, g))
       } ^ end ^
    "'Not' selectors produce negated Filters" ! {
      "selector"       | "filter"      |
      Not(Accept)      ! Some(EXCLUDE) |
      Not(Not(Accept)) ! Some(INCLUDE) |
      Accept           ! Some(INCLUDE) |> {
        (sel, filt) => sel.filterOpt must_== filt
      }
    } ^ end ^
    "Spot checks on miscellaneous filter combos" ! {
      "selector"                    | "criterion"                           |
      expr1                         ! beAnInstanceOf[ogc.PropertyIsGreaterThan] |
      Not(expr1)                    ! beAnInstanceOf[ogc.Not]                   |
      OrSelector(List(expr1))       ! beAnInstanceOf[ogc.PropertyIsGreaterThan] |
      AndSelector(List(expr1))      ! beAnInstanceOf[ogc.PropertyIsGreaterThan] |
      Not(AndSelector(List(expr1))) ! beAnInstanceOf[ogc.Not] |
      OrSelector(List(AndSelector(List(expr1)))) ! beAnInstanceOf[ogc.PropertyIsGreaterThan] |
      OrSelector(List(AndSelector(List(Not(expr1))))) ! beAnInstanceOf[ogc.Not] |>
      { (sel, pass) => sel.filterOpt must beSome.which(_ must pass) }
    }
}
