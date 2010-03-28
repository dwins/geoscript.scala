package org.geoserver.community.css.filter

import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

class FilterTest extends FilterOps with JUnitSuite with MustMatchersForJUnit {
  def in(s: String) = getClass.getResourceAsStream(s)

  def equivalentTo(expected: Filter) = new BeMatcher[Filter] {
    def apply(actual: Filter) = 
      MatchResult(
        equivalent(expected, actual),
        "Found %s; expected %s".format(actual, expected),
        "Found expected filter: %s".format(actual)
      )
  }

  @Test def equivalentFilters {
    val testcases = List(
      ("EXCLUDE", "EXCLUDE"),
      ("INCLUDE", "INCLUDE"),
      ("A < 1", "A < 1")
    ) map { 
      case (lhs, rhs) => (ECQL.toFilter(lhs), ECQL.toFilter(rhs))
    }

    for ((lhs, rhs) <- testcases) lhs must be (equivalentTo(rhs))
  }

  @Test def redundancy {
    val testcases = List(
      ("A < 1", "A <> 1")
    ) map { case (lhs, rhs) => (ECQL.toFilter(lhs), ECQL.toFilter(rhs)) }

    for ((lhs, rhs) <- testcases) {
      redundant(lhs, rhs) must be (true)
    }
  }

  @Test def constrainFilters {
    val testcases = List(
      ("A < 1", "A > 1", "EXCLUDE"),
      ("A <= 1", "A > 1", "EXCLUDE"),
      ("A >= 1", "A < 1", "EXCLUDE"),
      ("A > 1", "A = 1", "EXCLUDE"),
      ("A = 1", "A <> 1", "EXCLUDE"),
      ("A < 1", "A <> 1", "A < 1"),
      ("A < 1", "A < 1", "A < 1"),
      ("A < 2", "A < 1", "A < 1"),
      ("A <= 1", "A < 1", "A < 1"),
      ("A LIKE 'abc%'", "A NOT LIKE 'abc%'", "EXCLUDE"),
      ("A > 2 AND A < 4", "A > 4", "EXCLUDE"),
      ("A > 2 OR A < 4", "A > 4", "A > 4")
    ) map {
      case (lhs, rhs, expected) => 
        (ECQL.toFilter(lhs), ECQL.toFilter(rhs), ECQL.toFilter(expected))
    }

    for ((lhs, rhs, expected) <- testcases) {
      constrain(lhs, rhs) must be (equivalentTo(expected))
      constrain(rhs, lhs) must be (equivalentTo(expected))
    }
  }
}
