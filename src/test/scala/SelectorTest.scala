package org.geoserver.community.css

import org.geotools.filter.text.ecql.ECQL
import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

class SelectorTest extends SelectorOps 
with JUnitSuite with MustMatchersForJUnit {
  def in(s: String) = getClass.getResourceAsStream(s)

  @Test def simplification {
    val any = AcceptSelector
    val id  = IdSelector("states.9")
    val cql = ExpressionSelector("STATE_NAME LIKE '%ia'")

    simplify(any :: id :: Nil) must be (id :: Nil)
    simplify(any :: cql :: id :: Nil) must be (cql :: id :: Nil)
    simplify(cql :: id :: Nil) must be (cql :: id :: Nil)
    simplify(cql :: NotSelector(cql) :: Nil) must be (Exclude :: Nil)

    simplify(
      WrappedFilter(ECQL.toFilter("PERSONS >= 4")) ::
      ExpressionSelector("PERSONS <  4") ::
      ExpressionSelector("PERSONS >  2") :: Nil
    ) must be ( 
      Exclude :: Nil
    )

    simplify(
      List(
        "A<2", "A >= 2", "A < 4", "A >= 4"
      ) map ExpressionSelector
    ) must be (
      Exclude :: Nil
    )
  }

  @Test def simplifyScales {
    val maxscale = PseudoSelector("scale", "<", "140000")
    val minscale = PseudoSelector("scale", ">", "5000")
    val selectors = maxscale :: minscale :: Nil
    (constrain(maxscale, minscale) match {
      case Exclude(_) => false
      case _ => true
    }) must be (true)

    simplify(selectors) exists {
      case Exclude(_) => true
      case _ => false
    } must be (false)
  }

  @Test def scales {
    val a = ExpressionSelector("a=1")
    constrain(a, a) must be (a)
    simplify(a :: a :: Nil) must be (a :: Nil)
    simplify(a :: NotSelector(a) :: Nil) must be (Exclude :: Nil)
  }
}
