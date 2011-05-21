package org.geoscript.geocss

import org.geotools.filter.text.ecql.ECQL

import org.specs._

class SelectorTest extends Specification {
  import SelectorOps._
  
  def in(s: String) = getClass.getResourceAsStream(s)

  case class SelectorEquivalenceMatcher(expected: Selector)
  extends matcher.Matcher[Selector]
  {
    def apply(actual: => Selector) =
      (
        homologous(expected, actual),
        "selectors were equivalent",
        "%s was not equivalent to %s".format(actual, expected)
      )
  }

  def beEquivalentTo(sel: Selector) = SelectorEquivalenceMatcher(sel)

  case class ContentMatcher(expected: Seq[Selector])
  extends matcher.Matcher[Seq[Selector]]
  {
    def apply(actual: => Seq[Selector]) = {
      val test =
        expected.length == actual.length &&
        (expected.forall(e => actual.exists(a => equivalent(e, a))))

      Triple(
        test,
        "content matched",
        "%s has different content from %s".format(actual, expected)
      )
    }
  }

  def haveContent(expected: Selector*) = ContentMatcher(expected)
  def haveContent(expected: Iterable[Selector]) = ContentMatcher(expected.toSeq)

  "we should be able to detect redundant selectors" in {
    val e = ExpressionSelector
    isSubSet(e("INCLUDE"), e("A = 1")) must beTrue
    isSubSet(e("A <= 4"), e("A <= 2")) must beTrue
    isSubSet(e("A >= 2"), e("A >= 4")) must beTrue

    isSubSet(e("A = 1"), e("INCLUDE")) must beFalse
    isSubSet(e("A >= 4"), e("A >= 2")) must beFalse

    e("EXCLUDE") must_== (e("EXCLUDE"))
    isSubSet(e("EXCLUDE"), e("EXCLUDE")) must beTrue
    equivalent(e("EXCLUDE"), e("EXCLUDE")) must beTrue
  }

  "we should be able to simplify selectors" in {
    val any = AcceptSelector
    val id  = IdSelector("states.9")
    val cql = ExpressionSelector("STATE_NAME LIKE '%ia'")

    simplify(id :: Nil) must haveContent(id)
    simplify(any :: id :: Nil) must haveContent(id)
    simplify(any :: cql :: id :: Nil) must haveContent(cql, id)
    simplify(cql :: id :: Nil) must haveContent(cql, id)
    simplify(cql :: NotSelector(cql) :: Nil) must haveContent(Exclude)
    simplify(Nil) must haveContent(Exclude)

    simplify(
      WrappedFilter(ECQL.toFilter("PERSONS >= 4")) ::
      ExpressionSelector("PERSONS <  4") ::
      ExpressionSelector("PERSONS >  2") :: Nil
    ) must haveContent(Exclude)

    simplify(
      List(
        "A<2", "A >= 2", "A < 4", "A >= 4"
      ) map ExpressionSelector
    ) must haveContent(Exclude)

    simplify(
      List("A >= 2", "A<4") map ExpressionSelector
    ) must haveContent(List("A >= 2", "A<4") map ExpressionSelector)

    simplify(
      List("A >= 2", "A >= 4") map ExpressionSelector
    ) must haveContent(List("A >= 4") map ExpressionSelector)

    simplify(
      List("A >= 2", "A < 2") map ExpressionSelector
    ) must haveContent(Exclude)

    simplify(
      List("A>=2", "A<4", "A>=4", "A >= 2") map ExpressionSelector
    ) must haveContent(Exclude)

    simplify(
      List("P < 2", "P >= 4", "INCLUDE", "P >= 2 AND P < 4", "INCLUDE") map ExpressionSelector
    ) must haveContent(Exclude)

    // commented out because Filter.equals() gives a false negative on this test
    simplify(
      List(
        "PERSONS < 2000000", "INCLUDE", "PERSONS < 2000000 OR PERSONS >= 4000000", "PERSONS < 4000000", "INCLUDE"
      ) map ExpressionSelector
    ) must haveContent(ExpressionSelector("PERSONS < 2000000"))

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        ExpressionSelector("waterway='riverbank'")
      )
    ) must haveContent(
      ExpressionSelector("natural='wetland'"),
      ExpressionSelector("waterway='riverbank'")
    )

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(
          ExpressionSelector("waterway='riverbank'"),
          ExpressionSelector("natural='wetland'")
        ))
      )
    ) must haveContent(
      ExpressionSelector("natural='wetland'")
    )

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(
          ExpressionSelector("natural IS NULL"),
          ExpressionSelector("natural='wetland'")
        ))
      )
    ) must haveContent(
      ExpressionSelector("natural='wetland'")
    )

    simplify(
      AndSelector(Seq(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(
          ExpressionSelector("waterway<>'riverbank'"),
          ExpressionSelector("waterway is null")
        ))
      ))
    ) must beEquivalentTo(
      OrSelector(List(
        AndSelector(List(
          ExpressionSelector("natural='wetland'"),
          ExpressionSelector("waterway<>'riverbank'")
        )),
        AndSelector(List(
          ExpressionSelector("natural='wetland'"),
          ExpressionSelector("waterway is null")
        ))
      ))
    )

    simplify(
      List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(ExpressionSelector("natural<>'water'"), ExpressionSelector("natural is null")))
      )
    ) must haveContent(
      ExpressionSelector("natural='wetland'")
    )

    simplify(
      AndSelector(List(
        ExpressionSelector("natural='wetland'"),
        OrSelector(List(ExpressionSelector("natural<>'water'"), ExpressionSelector("natural is null"))),
        OrSelector(List(ExpressionSelector("waterway<>'riverbank'"), ExpressionSelector("waterway is null")))
      ))
    ) must beEquivalentTo(
      OrSelector(List(
        AndSelector(List(
          ExpressionSelector("natural='wetland'"),
          ExpressionSelector("waterway<>'riverbank'")
        )),
        AndSelector(List(
          ExpressionSelector("natural='wetland'"),
          ExpressionSelector("waterway is null")
        ))
      ))
    )

  }

  "contextual filters such as scales should also be simplifiable" in {
    val maxscale = PseudoSelector("scale", "<", "140000")
    val minscale = PseudoSelector("scale", ">", "5000")
    val selectors = maxscale :: minscale :: Nil
    constrain(maxscale, minscale) must_== (maxscale)
    constrainOption(maxscale, minscale) must beNone
    constrain(minscale, maxscale) must_== (minscale)
    constrainOption(minscale, maxscale) must beNone
    simplify(selectors) must haveContent(selectors)
    intersection(minscale, SelectorOps.not(minscale)) must beSome(Empty)
  }

  "normalize selectors to a canonical form" in {
    val comparison = ExpressionSelector("A < 1")
    val comparison2 = ExpressionSelector("B > 2")

    "simple selectors" >> {
      normalize(AcceptSelector) must_==
        OrSelector(Seq(AndSelector(Seq(AcceptSelector))))

      normalize(Exclude) must_==
        OrSelector(Seq(AndSelector(Seq(Exclude))))

      normalize(comparison) must_==
        OrSelector(Seq(AndSelector(Seq(comparison))))
    }

    "ANDed simple selectors" >> {
      normalize(AndSelector(Seq(AcceptSelector))) must_==
        OrSelector(Seq(AndSelector(Seq(AcceptSelector))))

      normalize(AndSelector(Seq(Exclude))) must_==
        OrSelector(Seq(AndSelector(Seq(Exclude))))

      normalize(AndSelector(Seq(comparison))) must_==
        OrSelector(Seq(AndSelector(Seq(comparison))))

      normalize(AndSelector(Seq(comparison, comparison2))) must_==
        OrSelector(Seq(AndSelector(Seq(comparison, comparison2))))
    }

    "ORed simple selectors" >> {
      normalize(OrSelector(Seq(AcceptSelector))) must_==
        OrSelector(Seq(AndSelector(Seq(AcceptSelector))))

      normalize(OrSelector(Seq(Exclude))) must_==
        OrSelector(Seq(AndSelector(Seq(Exclude))))

      normalize(OrSelector(Seq(comparison))) must_==
        OrSelector(Seq(AndSelector(Seq(comparison))))

      normalize(OrSelector(Seq(comparison, comparison2))) must_==
        OrSelector(Seq(
          AndSelector(Seq(comparison)),
          AndSelector(Seq(comparison2))
        ))
    }

    "ORed ANDs" >> {
      normalize(
        OrSelector(Seq(
          AndSelector(Seq(
            AcceptSelector,
            AcceptSelector
          ))
        ))
      ) must_==
        OrSelector(Seq(
          AndSelector(Seq(
            AcceptSelector,
            AcceptSelector
          ))
        ))

      normalize(
        OrSelector(Seq(
          AndSelector(Seq(
            Exclude,
            Exclude
          ))
        ))
      ) must_==
        OrSelector(Seq(
          AndSelector(Seq(
            Exclude,
            Exclude
          ))
        ))

      normalize(
        OrSelector(Seq(
          AndSelector(Seq(
            comparison,
            comparison2
          ))
        ))
      ) must_==
        OrSelector(Seq(
          AndSelector(Seq(
            comparison,
            comparison2
          ))
        ))
    }

    "ANDed ORs" >> {
      normalize(
        AndSelector(Seq(
          OrSelector(Seq(
            AcceptSelector,
            AcceptSelector
          ))
        ))
      ) must_==
        OrSelector(Seq(
          AndSelector(Seq(AcceptSelector)),
          AndSelector(Seq(AcceptSelector))
        ))

      normalize(
        AndSelector(Seq(
          OrSelector(Seq(
            Exclude,
            Exclude
          ))
        ))
      ) must_==
        OrSelector(Seq(
          AndSelector(Seq(Exclude)),
          AndSelector(Seq(Exclude))
        ))

      normalize(
        AndSelector(Seq(
          OrSelector(Seq(
            comparison,
            comparison2
          ))
        ))
      ) must_==
        OrSelector(Seq(
          AndSelector(Seq(comparison)),
          AndSelector(Seq(comparison2))
        ))
    }
  }

  "pseudoclasses should not have equivalent filters" in {
    PseudoClass("mark").filterOpt must beNone
  }
}
