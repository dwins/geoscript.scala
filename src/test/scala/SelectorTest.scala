package org.geoserver.community.css

import org.geotools.filter.text.ecql.ECQL
import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.scalatest.matchers.{HavePropertyMatcher, HavePropertyMatchResult}
import org.junit.Test

class SelectorTest extends SelectorOps 
with JUnitSuite with MustMatchersForJUnit {
  def in(s: String) = getClass.getResourceAsStream(s)

  def content(expected: Selector*)
  : HavePropertyMatcher[List[Selector], List[Selector]] =
    content(expected.toList)

  def content(expected: List[Selector]) =
    // TODO: Make selector.equals better at this
    new HavePropertyMatcher[List[Selector], List[Selector]] {
      def apply(actual: List[Selector]) = 
        HavePropertyMatchResult(
          Set(expected: _*) == Set(actual: _*),
          "content",
          expected,
          actual
        )
    }

  @Test def redundancy {
    val testcases = List(
      ("A = 1", "INCLUDE")
    ) map { case (l, r) => (ExpressionSelector(l), ExpressionSelector(r)) }

    for ((lhs, rhs) <- testcases) {
      redundant(lhs, rhs) must be (true)
    }
  }

  @Test def simplification {
    val any = AcceptSelector
    val id  = IdSelector("states.9")
    val cql = ExpressionSelector("STATE_NAME LIKE '%ia'")

    // simplify(any :: id :: Nil) must have (content(id))
    // simplify(any :: cql :: id :: Nil) must have (content(cql, id))
    // simplify(cql :: id :: Nil) must have (content(cql, id))
    simplify(cql :: NotSelector(cql) :: Nil) must have (content(Exclude))

    simplify(
      WrappedFilter(ECQL.toFilter("PERSONS >= 4")) ::
      ExpressionSelector("PERSONS <  4") ::
      ExpressionSelector("PERSONS >  2") :: Nil
    ) must have (content(Exclude)) 

    simplify(
      List(
        "A<2", "A >= 2", "A < 4", "A >= 4"
      ) map ExpressionSelector
    ) must have (content(Exclude)) 

    simplify(
      List("A>=2", "A<4", "A>=4", "A >= 2") map ExpressionSelector
    ) must have (content(Exclude))
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
    // constrain(a, a) must be (a)
    // simplify(a :: a :: Nil) must be (a :: Nil)
    simplify(a :: NotSelector(a) :: Nil) must be (Exclude :: Nil)
  }
}
