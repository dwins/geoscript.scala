package org.geoscript.geocss
package filter

import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL
import scala.collection.JavaConversions._

import org.scalatest.{ FunSuite, OptionValues }
import org.scalatest.matchers.ShouldMatchers

class FilterTest extends FunSuite with ShouldMatchers with OptionValues {
  import ECQL.toFilter
  import org.geoscript.support.logic.Knowledge.{ Absurdity, Oblivion }
  val kb = Oblivion[Filter]
  import kb.{ reduce, given }

  def equivalentTo(a: Filter): org.scalatest.matchers.BeMatcher[Filter] =
    new org.scalatest.matchers.BeMatcher[Filter] {
      override def apply(left: Filter): org.scalatest.matchers.MatchResult =
        org.scalatest.matchers.MatchResult(
          equivalent(a, left),
          "%s was not equivalent to %s" format(left, a),
          "%s equivalent to %s" format(left, a)
        )
    }

  def haveAsSubset(a: Filter): org.scalatest.matchers.Matcher[Filter] =
    new org.scalatest.matchers.Matcher[Filter] {
      override def apply(f: Filter): org.scalatest.matchers.MatchResult =
        new org.scalatest.matchers.MatchResult(
          isSubSet(f, a),
          "%s is not superset of %s" format(f, a),
          "%s has subset %s" format(f, a)
        )
    }

  def intersection(f: Filter, g: Filter): Option[Filter] = {
    val and = FiltersAreSentential.and(f, g)
    Option(reduce(and)).filter(and !=)
  }

  def union(f: Filter, g: Filter): Option[Filter] = {
    val or = FiltersAreSentential.or(f, g)
    Some(reduce(or)).filter(or !=)
  }

  def constrain(f: Filter, g: Filter): Filter = {
    val and = FiltersAreSentential.and(f, g)
    reduce(and)
  }

  def equivalent(f: Filter, g: Filter): Boolean = 
    (reduce(f) == reduce(g)) ||
    (given(f).reduce(g) == Filter.INCLUDE) &&
    (given(g).reduce(f) == Filter.INCLUDE)

  def isSubSet(f: Filter, g: Filter): Boolean =
    reduce(g) == Filter.EXCLUDE ||
    given(g) != Absurdity &&
    given(g).reduce(f) == Filter.INCLUDE

  test("Equivalence") {
    toFilter("EXCLUDE") should be (equivalentTo(toFilter("EXCLUDE")))
    toFilter("INCLUDE") should be (equivalentTo(toFilter("INCLUDE")))
    toFilter("A < 1") should be (equivalentTo(toFilter("A < 1")))
    toFilter("A <> 1") should be (equivalentTo(toFilter("A <> 1")))
    toFilter("A IS NULL") should be (equivalentTo(toFilter("A IS NULL")))
    toFilter("A <= 1 OR B >= 2") should be (equivalentTo(toFilter("A <= 1 OR B >= 2")))
    toFilter("A <= 1 AND B >= 2") should be (equivalentTo(toFilter("A <= 1 AND B >= 2")))

    toFilter("A < 1") should not be (equivalentTo(toFilter("A > 1")))
    toFilter("A < 1") should not be (equivalentTo(toFilter("A = 1")))
    toFilter("A = 1") should not be (equivalentTo(toFilter("A > 1")))
  }

  test("Subset") {
    toFilter("A >= 1") should haveAsSubset(toFilter("A = 2"))
    toFilter("A < 1") should haveAsSubset(toFilter("A < 1"))
    toFilter("A < 2") should haveAsSubset(toFilter("A < 1"))
    toFilter("A IS NOT NULL") should haveAsSubset(toFilter("A < 1"))
    toFilter("A <> 2") should haveAsSubset(toFilter("A < 2"))
    toFilter("A <> 2") should haveAsSubset(toFilter("A < 1"))
    toFilter("A < 1") should haveAsSubset(toFilter("A < 1 AND A <> 2"))
    toFilter("A < 1") should haveAsSubset(toFilter("A < 1 AND A > 0"))
    toFilter("A <> 1") should haveAsSubset(toFilter("A < 1"))
    toFilter("A <> 1 OR A IS NULL") should haveAsSubset(toFilter("A IS NULL"))
    toFilter("A = 1 OR A = 2") should haveAsSubset(toFilter("A = 1 OR A = 2"))
    toFilter("A <> 1 OR A IS NULL") should haveAsSubset(toFilter("A < 1 OR A IS NULL"))

    toFilter("A = 1") should not (haveAsSubset(toFilter("A <> 1")))
    toFilter("A = 1") should not (haveAsSubset(toFilter("A < 1")))
    toFilter("A = 1") should not (haveAsSubset(toFilter("A <= 1")))
    toFilter("A = 1") should not (haveAsSubset(toFilter("A > 1")))
    toFilter("A = 1") should not (haveAsSubset(toFilter("A >= 1")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A = 1")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A < 2")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A <= 1")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A <= 2")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A > 0")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A >= 1")))
    toFilter("A <> 1") should not (haveAsSubset(toFilter("A >= 0")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A = 1")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A <> 1")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A <> 0")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A <> 2")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A <= 1")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A <= 2")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A > 1")))
    toFilter("A < 1") should not (haveAsSubset(toFilter("A >= 1")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A = 2")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A <> 1")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A <> 0")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A <> 2")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A < 2")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A <= 2")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A > 1")))
    toFilter("A <= 1") should not (haveAsSubset(toFilter("A >= 1")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A = 1")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A = 0")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A <> 1")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A < 1")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A <= 1")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A > 0")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A >= 1")))
    toFilter("A > 1") should not (haveAsSubset(toFilter("A >= 0")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A = 0")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A <> 1")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A <> 0")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A <> 2")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A < 1")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A <= 1")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A > 0")))
    toFilter("A >= 1") should not (haveAsSubset(toFilter("A >= 0")))
    toFilter("A IS NULL") should not (haveAsSubset(toFilter("A IS NOT NULL")))
    toFilter("A IS NOT NULL") should not (haveAsSubset(toFilter("A IS NULL")))

    toFilter("A = 1") should (haveAsSubset(toFilter("A = 1")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A = 2")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A <> 1")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A < 1")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A < 0")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A <= 0")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A > 1")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A > 2")))
    toFilter("A <> 1") should (haveAsSubset(toFilter("A >= 2")))
    toFilter("A < 1") should (haveAsSubset(toFilter("A = 0")))
    toFilter("A < 1") should (haveAsSubset(toFilter("A < 1")))
    toFilter("A < 1") should (haveAsSubset(toFilter("A <= 0")))
    toFilter("A <= 1") should (haveAsSubset(toFilter("A = 1")))
    toFilter("A <= 1") should (haveAsSubset(toFilter("A = 0")))
    toFilter("A <= 1") should (haveAsSubset(toFilter("A < 1")))
    toFilter("A <= 1") should (haveAsSubset(toFilter("A < 0")))
    toFilter("A <= 1") should (haveAsSubset(toFilter("A <= 1")))
    toFilter("A <= 1") should (haveAsSubset(toFilter("A <= 0")))
    toFilter("A > 1") should (haveAsSubset(toFilter("A = 2")))
    toFilter("A > 1") should (haveAsSubset(toFilter("A > 1")))
    toFilter("A > 1") should (haveAsSubset(toFilter("A > 2")))
    toFilter("A > 1") should (haveAsSubset(toFilter("A >= 2")))
    toFilter("A >= 1") should (haveAsSubset(toFilter("A = 1")))
    toFilter("A >= 1") should (haveAsSubset(toFilter("A = 2")))
    toFilter("A >= 1") should (haveAsSubset(toFilter("A > 1")))
    toFilter("A >= 1") should (haveAsSubset(toFilter("A > 2")))
    toFilter("A >= 1") should (haveAsSubset(toFilter("A >= 1")))
    toFilter("A >= 1") should (haveAsSubset(toFilter("A >= 2")))
    toFilter("A IS NULL") should (haveAsSubset(toFilter("A IS NULL")))
    toFilter("A IS NOT NULL") should (haveAsSubset(toFilter("A = 1")))
    toFilter("A IS NOT NULL") should (haveAsSubset(toFilter("A <> 1")))
  }

  test("Intersection") {
    // "A >= 1"), toFilter("A <= 1" !! "A = 1"|
    intersection(toFilter("A < 1 OR A IS NULL"), toFilter("A IS NOT NULL")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A < 1 OR A IS NULL"), toFilter("A IS NOT NULL")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A < 1 OR A IS NULL"), toFilter("A < 1 OR A IS NULL")).value should be (equivalentTo(toFilter("A < 1 OR A IS NULL")))
    intersection(toFilter("A < 1 OR A IS NULL"), toFilter("A <> 1 OR A IS NULL")).value should be (equivalentTo(toFilter("A < 1 OR A IS NULL")))
    intersection(toFilter("A < 1"), toFilter("A > 1")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A <= 1"), toFilter("A > 1")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A >= 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A = 1"), toFilter("A > 1")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A > 1"), toFilter("A = 1")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A = 1"), toFilter("A <> 1")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A > 2"), toFilter("A >= 2")).value should be (equivalentTo(toFilter("A > 2")))
    intersection(toFilter("A < 2"), toFilter("A <= 2")).value should be (equivalentTo(toFilter("A < 2")))
    intersection(toFilter("A < 1"), toFilter("A <> 1")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A <> 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A < 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A < 2"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A <= 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A < 1")))
    intersection(toFilter("A LIKE 'abc%'"), toFilter("A NOT LIKE 'abc%'")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A > 2 AND A < 4"), toFilter("A > 4")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A IS NULL"), toFilter("A IS NOT NULL")).value should be (equivalentTo(toFilter("EXCLUDE")))
    intersection(toFilter("A > 2"), toFilter("A IS NOT NULL")).value should be (equivalentTo(toFilter("A > 2")))
    intersection(toFilter("A IS NOT NULL"), toFilter("A > 2")).value should be (equivalentTo(toFilter("A > 2")))
    intersection(toFilter("A > 2"), toFilter("A IS NULL OR A < 10")).value should be (equivalentTo(toFilter("A > 2 AND A < 10")) )
  }

  test("Constraint") {
    constrain(toFilter("A > 2 OR A < 4"), toFilter("A > 4")) should be (equivalentTo(toFilter("A > 4")))
    constrain(toFilter("PERSONS >= 4000000"), toFilter("PERSONS < 4000000")) should be (equivalentTo(toFilter("EXCLUDE")))
  }

  test("Union") {
    union(toFilter("A < 4"), toFilter("A > 1")).value should be (equivalentTo(toFilter("INCLUDE")))
    union(toFilter("A < 4"), toFilter("A > 1")).value should be (equivalentTo(toFilter("INCLUDE")))
    union(toFilter("A = 1"), toFilter("A <> 1")).value should be (equivalentTo(toFilter("INCLUDE")))
    union(toFilter("A >= 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("INCLUDE")))
    union(toFilter("A <= 1"), toFilter("A > 1")).value should be (equivalentTo(toFilter("INCLUDE")))
    union(toFilter("A < 1"), toFilter("A <> 1")).value should be (equivalentTo(toFilter("A <> 1")))
    union(toFilter("A <> 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A <> 1")))
    union(toFilter("A < 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A < 1")))
    union(toFilter("A < 2"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A < 2")))
    union(toFilter("A <= 1"), toFilter("A < 1")).value should be (equivalentTo(toFilter("A <= 1")))
    union(toFilter("A > 2 AND A <= 4"), toFilter("A > 4")).value should be (equivalentTo(toFilter("A > 2")))
    union(toFilter("A <= 2 OR A > 4"), toFilter("A > 4")).value should be (equivalentTo(toFilter("A <= 2 OR A > 4")))
  }

  test("Reduce") {
    reduce(toFilter("(A > 1 AND A < 3) OR (B > 1 AND B < 3)")) should be (equivalentTo(toFilter("(A > 1 AND A < 3) OR (B > 1 AND B < 3)")))
    reduce(toFilter("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)")) should be (equivalentTo(toFilter("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)")))
    reduce(toFilter("A <> 1")) should be (equivalentTo(toFilter("A IS NULL OR A <> 1")))
    reduce(toFilter("A >= 1 OR A IS NULL OR A = 2")) should be (equivalentTo(toFilter("A >= 1 OR A IS NULL")))
    reduce(toFilter("A = 1 AND (B = 2 OR A = 1)")) should be (equivalentTo(toFilter("A = 1")))
    reduce(toFilter("A = 1 AND B = 2 AND A = 1")) should be (equivalentTo(toFilter("A = 1 AND B = 2")))
    reduce(toFilter("A < 1 AND A < 1")) should be (equivalentTo(toFilter("A < 1")))
    reduce(toFilter("A > 2 AND A < 1")) should be (equivalentTo(toFilter("EXCLUDE")))
    reduce(toFilter("A < 1 AND A > 2")) should be (equivalentTo(toFilter("EXCLUDE")))
    reduce(toFilter("A IS NULL AND A IS NOT NULL")) should be (equivalentTo(toFilter("EXCLUDE")))
    reduce(toFilter("A <> 2 AND A IS NOT NULL")) should be (equivalentTo(toFilter("A <> 2")))
    reduce(toFilter("(A <> 2 OR A IS NULL) AND A IS NOT NULL")) should be (equivalentTo(toFilter("A <> 2")))
    reduce(toFilter("A < 1 AND A <> 2")) should be (equivalentTo(toFilter("A < 1")))
    reduce(toFilter("A <= 1 OR A IS NULL OR A <= 1 OR A IS NULL")) should be (equivalentTo(toFilter("A <=1 OR A IS NULL")))
    reduce(toFilter("A > 15 AND (A < 20 OR A IS NULL)")) should be (equivalentTo(toFilter("A > 15 AND A < 20")))
    // reduce(toFilter("(A <> 2 OR A IS NULL) AND (A < 1 OR A IS NULL) AND A IS NOT NULL")) should be (equivalentTo(toFilter("A < 1")))
  }
}
