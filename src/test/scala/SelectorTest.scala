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
    val positivecases = List(
      ("INCLUDE", "A = 1"),
      ("A <= 4", "A <= 2"),
      ("A >= 2", "A >= 4")
    ) map { case (l, r) => (ExpressionSelector(l), ExpressionSelector(r)) }

    for ((lhs, rhs) <- positivecases) {
      redundant(lhs, rhs) must be (true)
    }

    val negativecases = List(
      ("A = 1", "INCLUDE"),
      ("A >= 4", "A >= 2")
    ) map { case (l, r) => (ExpressionSelector(l), ExpressionSelector(r)) }

    for ((lhs, rhs) <- negativecases) {
      redundant(lhs, rhs) must be (false)
    }
  }

  @Test def simplification {
    val any = AcceptSelector
    val id  = IdSelector("states.9")
    val cql = ExpressionSelector("STATE_NAME LIKE '%ia'")

    simplify(any :: id :: Nil) must have (content(id))
    simplify(any :: cql :: id :: Nil) must have (content(cql, id))
    simplify(cql :: id :: Nil) must have (content(cql, id))
    simplify(cql :: NotSelector(cql) :: Nil) must have (content(Exclude))
    simplify(Nil) must have (content(Exclude))

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
      List("A >= 2", "A<4") map ExpressionSelector
    ) must have (content(List("A >= 2", "A<4") map ExpressionSelector))

    simplify(
      List("A >= 2", "A >= 4") map ExpressionSelector
    ) must have (content(List("A >= 4") map ExpressionSelector))

    simplify(
      List("A >= 2", "A < 2") map ExpressionSelector
    ) must have (content(Exclude))

    simplify(
      List("A>=2", "A<4", "A>=4", "A >= 2") map ExpressionSelector
    ) must have (content(Exclude))

    simplify(
      List("P < 2", "P >= 4", "INCLUDE", "P >= 2 AND P < 4", "INCLUDE") map ExpressionSelector
    ) must have (content(Exclude))

    // commented out because Filter.equals() gives a false negative on this test
    // simplify(
    //   List(
    //     "PERSONS < 2000000", "INCLUDE", "PERSONS < 2000000 OR PERSONS >= 4000000", "PERSONS < 4000000", "INCLUDE"
    //   ) map ExpressionSelector
    // ) must have (content(ExpressionSelector("PERSONS < 2000000")))
  }

  @Test def simplifyScales {
    val maxscale = PseudoSelector("scale", "<", "140000")
    val minscale = PseudoSelector("scale", ">", "5000")
    val selectors = maxscale :: minscale :: Nil
    constrain(maxscale, minscale) must be (maxscale)
    constrainOption(maxscale, minscale) must be (None)
    constrain(minscale, maxscale) must be (minscale)
    constrainOption(minscale, maxscale) must be (None)
    simplify(selectors) must have (content(selectors))
  }
}
