package org.geoserver.community.css

import org.geotools.filter.text.ecql.ECQL
import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.scalatest.matchers.{HavePropertyMatcher, HavePropertyMatchResult}
import org.junit.Test

class SelectorTest extends JUnitSuite with MustMatchersForJUnit {
  import SelectorOps._
  
  def in(s: String) = getClass.getResourceAsStream(s)

  def content(expected: Selector*)
  : HavePropertyMatcher[List[Selector], List[Selector]] =
    content(expected.toList)

  def content(expected: List[Selector]) =
    new HavePropertyMatcher[List[Selector], List[Selector]] {
      def apply(actual: List[Selector]) = {
        val test = expected.length == actual.length && 
          (expected.forall(e => actual.exists(a => equivalent(e, a))))
        HavePropertyMatchResult(test, "content", expected, actual)
      }
    }

  @Test def redundancy {
    val e = ExpressionSelector
    isSubSet(e("INCLUDE"), e("A = 1")) must be (true)
    isSubSet(e("A <= 4"), e("A <= 2")) must be (true)
    isSubSet(e("A >= 2"), e("A >= 4")) must be (true)

    isSubSet(e("A = 1"), e("INCLUDE")) must be (false)
    isSubSet(e("A >= 4"), e("A >= 2")) must be (false)

    e("EXCLUDE") must be (e("EXCLUDE"))
    isSubSet(e("EXCLUDE"), e("EXCLUDE")) must be (true)
    equivalent(e("EXCLUDE"), e("EXCLUDE")) must be (true)
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
    simplify(
      List(
        "PERSONS < 2000000", "INCLUDE", "PERSONS < 2000000 OR PERSONS >= 4000000", "PERSONS < 4000000", "INCLUDE"
      ) map ExpressionSelector
    ) must have (content(ExpressionSelector("PERSONS < 2000000")))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        ExpressionSelector("waterway='riverbank'")
      )
    ) must have (content(
      ExpressionSelector("natural='wetland'"),
      ExpressionSelector("waterway='riverbank'")
    ))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(
          ExpressionSelector("waterway='riverbank'"),
          ExpressionSelector("natural='wetland'")
        ))
      )
    ) must have (content(
      ExpressionSelector("natural='wetland'")
    ))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(
          ExpressionSelector("natural IS NULL"),
          ExpressionSelector("natural='wetland'")
        ))
      )
    ) must have (content(
      ExpressionSelector("natural='wetland'")
    ))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(ExpressionSelector("waterway<>'riverbank'"), ExpressionSelector("waterway is null")))
      )
    ) must have (content(
      ExpressionSelector("natural='wetland'"), 
      OrSelector(List(ExpressionSelector("waterway<>'riverbank'"), ExpressionSelector("waterway is null")))
    ))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(ExpressionSelector("natural<>'water'"), ExpressionSelector("natural is null")))
      )
    ) must have (content(
      ExpressionSelector("natural='wetland'") 
    ))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(ExpressionSelector("natural<>'water'"), ExpressionSelector("natural is null"))),
        OrSelector(List(ExpressionSelector("waterway<>'riverbank'"), ExpressionSelector("waterway is null")))
      )
    ) must have (content(
      ExpressionSelector("natural='wetland'"), 
      OrSelector(List(ExpressionSelector("waterway<>'riverbank'"), ExpressionSelector("waterway is null")))
    ))
//(natural='wetland' or natural is null) or (natural='wetland' or natural is null)
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
    intersection(minscale, SelectorOps.not(minscale)) must be (Some(Empty))
  }
}
