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
    import ECQL.{ toFilter => f }
    f("EXCLUDE") must be (equivalentTo(f("EXCLUDE")))
    f("INCLUDE") must be (equivalentTo(f("INCLUDE")))
    f("A < 1") must be (equivalentTo(f("A < 1")))
    f("A <> 1") must be (equivalentTo(f("A <> 1")))
    f("A IS NULL") must be (equivalentTo(f("A IS NULL")))
    f("A <= 1 OR B >= 2") must be (equivalentTo(f("A <= 1 OR B >= 2")))
    f("A <= 1 AND B >= 2") must be (equivalentTo(f("A <= 1 AND B >= 2")))

    f("A < 1") must not be (equivalentTo(f("A > 1")))
    f("A < 1") must not be (equivalentTo(f("A = 1")))
    f("A = 1") must not be (equivalentTo(f("A > 1")))
  }

  @Test def negation {
    import ECQL.{ toFilter => f }
    negate(f("A < 1")) must be (equivalentTo(f("A >= 1 OR A IS NULL")))
    negate(f("A > 1")) must be (equivalentTo(f("A <= 1 OR A IS NULL")))
    negate(f("A <= 1")) must be (equivalentTo(f("A > 1 OR A IS NULL")))
    negate(f("A >= 1")) must be (equivalentTo(f("A < 1 OR A IS NULL")))
    negate(f("A > 1 AND B < 2")) must be (equivalentTo(f("A <= 1 OR A IS NULL OR B >= 2 OR B IS NULL")))
    // TODO: figure out and implement a canonical form for nested logical filters
    // negate(f("A > 1 OR B < 2")) must be (equivalentTo(f("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)")))
    simplify(negate(f("A < 1 AND A <> 2"))) must be (equivalentTo(f("A IS NULL OR A >= 1")))
    simplify(negate(negate(f("A < 1 AND A <> 2")))) must be (equivalentTo(f("A < 1")))
  }

  @Test def redundancy {
    import ECQL.{ toFilter => f }
    redundant(f("A = 2"), f("A >= 1")) must be (true)
    redundant(f("A > 1"), f("A = 1")) must be (false)
    redundant(f("A = 1"), f("A > 1")) must be (false)
    redundant(f("A < 1"), f("A < 1")) must be (true)
    redundant(f("A < 2"), f("A < 1")) must be (true)
    redundant(f("A < 1"), f("A < 2")) must be (false)
    redundant(f("A < 1 OR B > 2"), f("A < 1")) must be (true)
    redundant(f("A < 0 OR B > 2"), f("A < 1")) must be (false)
    redundant(f("A IS NOT NULL"), f("A < 1")) must be (true)
    redundant(f("A <> 2"), f("A < 1")) must be (true)
    redundant(f("A < 1 AND A <> 2"), f("A < 1")) must be (true)
    redundant(f("A < 1 AND A > 0"), f("A < 1")) must be (false)
  }

  @Test def constrainFilters {
    import ECQL.{ toFilter => f }
    constrain(f("A < 1 OR A IS NULL"), f("A < 1 OR A IS NULL")) must be (equivalentTo(f("A < 1 OR A IS NULL")))
    simplify(constrain(f("A < 1 OR A IS NULL"), f("A <> 1 OR A IS NULL"))) must be (equivalentTo(f("A < 1 OR A IS NULL")))
    constrain(f("A < 1"), f("A > 1")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A <= 1"), f("A > 1")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A >= 1"), f("A < 1")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A = 1"), f("A > 1")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A > 1"), f("A = 1")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A = 1"), f("A <> 1")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A < 1"), f("A <> 1")) must be (equivalentTo(f("A < 1")))
    constrain(f("A <> 1"), f("A < 1")) must be (equivalentTo(f("A < 1")))
    constrain(f("A < 1"), f("A < 1")) must be (equivalentTo(f("A < 1")))
    constrain(f("A < 2"), f("A < 1")) must be (equivalentTo(f("A < 1")))
    constrain(f("A <= 1"), f("A < 1")) must be (equivalentTo(f("A < 1")))
    constrain(f("A LIKE 'abc%'"), f("A NOT LIKE 'abc%'")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A > 2 AND A < 4"), f("A > 4")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A > 2 OR A < 4"), f("A > 4")) must be (equivalentTo(f("A > 4")))
    constrain(f("PERSONS >= 4000000"), f("PERSONS < 4000000")) must be (equivalentTo(f("EXCLUDE")))
    constrain(f("A = 'bar'"), f("B = 'foo' or A = 'bar'")) must be (equivalentTo(f("A = 'bar'")))
  }

  @Test def relaxFilters {
    import ECQL.{ toFilter => f }
    relax(f("A <= 1"), f("A > 1")) must be (equivalentTo(f("INCLUDE")))
    relax(f("A < 1"), f("A <> 1")) must be (equivalentTo(f("A <> 1")))
    relax(f("A <> 1"), f("A < 1")) must be (equivalentTo(f("A <> 1")))
    relax(f("A >= 1"), f("A < 1")) must be (equivalentTo(f("INCLUDE")))
    relax(f("A > 1"), f("A = 1")) must be (equivalentTo(f("A >= 1")))
    relax(f("A = 1"), f("A <> 1")) must be (equivalentTo(f("INCLUDE")))
    relax(f("A < 1"), f("A < 1")) must be (equivalentTo(f("A < 1")))
    relax(f("A < 2"), f("A < 1")) must be (equivalentTo(f("A < 2")))
    relax(f("A < 4"), f("A > 1")) must be (equivalentTo(f("INCLUDE")))
    relax(f("A <= 1"), f("A < 1")) must be (equivalentTo(f("A <= 1")))
    relax(f("A < 1 OR A IS NULL"), f("A < 1 OR A IS NULL")) must be (equivalentTo(f("A < 1 OR A IS NULL")))
    relax(f("A < 1 OR A IS NULL"), f("A <> 1 OR A IS NULL")) must be (equivalentTo(f("A <> 1 OR A IS NULL")))
    relax(f("A LIKE 'abc%'"), f("A NOT LIKE 'abc%'")) must be (equivalentTo(f("INCLUDE")))
    relax(f("A > 2 AND A <= 4"), f("A > 4")) must be (equivalentTo(f("A > 2")))
    relax(f("A <= 2 OR A > 4"), f("A > 4")) must be (equivalentTo(f("A <= 2 OR A > 4")))
    relax(f("A > 2 OR A < 4"), f("A > 4")) must be (equivalentTo(f("INCLUDE")))
    relax(f("PERSONS >= 4000000"), f("PERSONS < 4000000")) must be (equivalentTo(f("INCLUDE")))
    relax(f("A = 'bar'"), f("B = 'foo' OR A = 'bar'")) must be (equivalentTo(f("A = 'bar' OR B = 'foo'")))
  }

  @Test def simplification {
    import ECQL.{ toFilter => f }
    // comment out the assertions for now, but we still want to at least check
    // that these terminate without a StackOverflowError
    simplify(f("(A > 1 AND A < 3) OR (B > 1 AND B < 3)")) // must be (equivalentTo(f("(A > 1 AND A < 3) OR (B > 1 AND B < 3)")))
    simplify(f("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)")) // must be (equivalentTo(f("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)")))

    // things that actually work now
    simplify(f("A >= 1 OR A IS NULL OR A = 2")) must be (equivalentTo(f("A >= 1 OR A IS NULL")))
    simplify(f("A <> 1")) must be (equivalentTo(filters.notEqual(filters.property("A"), filters.literal(1L))))
    simplify(f("A = 1 AND (B = 2 OR A = 1)")) must be (equivalentTo(f("A = 1")))
    simplify(f("A = 1 AND B = 2 AND A = 1")) must be (equivalentTo(f("A = 1 AND B = 2")))
    simplify(f("A < 1 AND A < 1")) must be (equivalentTo(f("A < 1")))
    simplify(f("A > 2 AND A < 1")) must be (equivalentTo(f("EXCLUDE")))
    simplify(f("A < 1 AND A > 2")) must be (equivalentTo(f("EXCLUDE")))
    simplify(f("A IS NULL AND A IS NOT NULL")) must be (equivalentTo(f("EXCLUDE")))
    simplify(f("(A <> 2 OR A IS NULL) AND A IS NOT NULL")) must be (equivalentTo(f("A <> 2")))
    simplify(f("NOT A < 1")) must be (equivalentTo(f("A >= 1")))
    simplify(f("A < 1 AND A <> 2")) must be (equivalentTo(f("A < 1")))
    simplify(f("(A <> 2 OR A IS NULL) AND (A < 1 OR A IS NULL) AND A IS NOT NULL")) must be (equivalentTo(f("A < 1")))
    simplify(f("A <= 1 OR A IS NULL OR A <= 1 OR A IS NULL")) must be (equivalentTo(f("A <=1 OR A IS NULL")))
  }
}
