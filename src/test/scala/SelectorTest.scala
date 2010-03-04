package org.geoserver.community.css

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

  @Test def simplifiedCount {
    val css = CssParser.parse(in("/filters.css")).get
    val sld = Translator.css2sld(css)
    sld.featureTypeStyles.get(0).rules.size must be (6)
  }

  @Test def scales {
    val a = ExpressionSelector("a=1")
    constrain(a, a) must be (a)
    simplify(a :: a :: Nil) must be (a :: Nil)
    simplify(a :: NotSelector(a) :: Nil) must be (Exclude :: Nil)
  }
}
