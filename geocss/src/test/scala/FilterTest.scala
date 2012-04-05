package org.geoscript.geocss
package filter

import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL

import org.specs2._

import scala.collection.JavaConversions._

class FilterTest extends Specification with matcher.DataTables {
  import ECQL.{ toFilter => f }

  def is =
    "basic filter equivalence tests should work" ^
      equivalenceTests ^ end ^
    "subset tests should be aware of binary operations" ^ 
      subsetTests ^ end ^
    "positive subset tests" ^ 
      positiveSubsetTests ^ end ^
    "negative subset tests" ^ 
      negativeSubsetTests ^ end ^
    "intersection tests" ^
      intersectionTests ^ end ^
    "it should be possible to simplify filters" ^
      simplifierTests ^ end ^
    "constraining filters should be aware of binary operations" ^
      constraintTests ^ end ^
    "unioning filters should be aware of binary operations" ^
      unionTests ^ end

  val equivalenceTests =
    "Basic equivalence tests" ! {
      "LHS"               || "RHS"               |
      "EXCLUDE"           !! "EXCLUDE"           | 
      "INCLUDE"           !! "INCLUDE"           |
      "A < 1"             !! "A < 1"             |
      "A <> 1"            !! "A <> 1"            |
      "A IS NULL"         !! "A IS NULL"         |
      "A <= 1 OR B >= 2"  !! "A <= 1 OR B >= 2"  |
      "A <= 1 AND B >= 2" !! "A <= 1 AND B >= 2" |> {
        (a, b) => f(a) must beEquivalentTo(f(b))
      }
    }

  val negativeEquivalenceTests =
    "equivalence tests that must produce false" ! {
      "LHS"   || "RHS"   |
      "A < 1" !! "A > 1" |
      "A < 1" !! "A = 1" |
      "A = 1" !! "A > 1" |> {
        (a, b) => f(a) must not(beEquivalentTo(f(b))) 
      }
    }

  val subsetTests = 
    "Cases where isSubSet should definitely give true" ! {
      "LHS"                 || "RHS"                |
      "INCLUDE"             !! "A = 2"              |
      "A >= 1"              !! "A = 2"              |
      "A < 1"               !! "A < 1"              |
      "A < 2"               !! "A < 1"              |
      "A IS NOT NULL"       !! "A < 1"              |
      "A <> 2"              !! "A < 2"              |
      "A <> 2"              !! "A < 1"              |
      "A < 1"               !! "A < 1 AND A <> 2"   |
      "A < 1"               !! "A < 1 AND A > 0"    |
      "A <> 1"              !! "A < 1"              |
      "A <> 1 OR A IS NULL" !! "A IS NULL"          |
      "A = 1 OR A = 2"      !! "A = 1 OR A = 2"     |
      "A <> 1 OR A IS NULL" !! "A < 1 OR A IS NULL" |> { (p, q) => 
        f(p) must haveAsSubset(f(q))
      }
    }

  val negativeSubsetTests =
    "Cases where isSubSet should definitely give false" ! {
      "LHS" || "RHS" |
      "A = 1" !! "A <> 1" |
      "A = 1" !! "A < 1" |
      "A = 1" !! "A <= 1" |
      "A = 1" !! "A > 1" |
      "A = 1" !! "A >= 1" |
      "A <> 1" !! "A = 1" |
      "A <> 1" !! "A < 2" |
      "A <> 1" !! "A <= 1" |
      "A <> 1" !! "A <= 2" |
      "A <> 1" !! "A > 0" |
      "A <> 1" !! "A >= 1" |
      "A <> 1" !! "A >= 0" |
      "A < 1" !! "A = 1" |
      "A < 1" !! "A <> 1" |
      "A < 1" !! "A <> 0" |
      "A < 1" !! "A <> 2" |
      "A < 1" !! "A <= 1" |
      "A < 1" !! "A <= 2" |
      "A < 1" !! "A > 1" |
      "A < 1" !! "A >= 1" |
      "A <= 1" !! "A = 2" |
      "A <= 1" !! "A <> 1" |
      "A <= 1" !! "A <> 0" |
      "A <= 1" !! "A <> 2" |
      "A <= 1" !! "A < 2" |
      "A <= 1" !! "A <= 2" |
      "A <= 1" !! "A > 1" |
      "A <= 1" !! "A >= 1" |
      "A > 1" !! "A = 1" |
      "A > 1" !! "A = 0" |
      "A > 1" !! "A <> 1" |
      "A > 1" !! "A < 1" |
      "A > 1" !! "A <= 1" |
      "A > 1" !! "A > 0" |
      "A > 1" !! "A >= 1" |
      "A > 1" !! "A >= 0" |
      "A >= 1" !! "A = 0" |
      "A >= 1" !! "A <> 1" |
      "A >= 1" !! "A <> 0" |
      "A >= 1" !! "A <> 2" |
      "A >= 1" !! "A < 1" |
      "A >= 1" !! "A <= 1" |
      "A >= 1" !! "A > 0" |
      "A >= 1" !! "A >= 0" |
      "A IS NULL" !! "A IS NOT NULL" |
      "A IS NOT NULL" !! "A IS NULL" |> {
        (lhs, rhs) => f(lhs) must not(haveAsSubset(f(rhs)))
      }
    }

  val positiveSubsetTests =
    "Cases where isSubSet should definitely give true" ! {
      "LHS" || "RHS" |
      "A = 1" !! "A = 1" |
      "A <> 1" !! "A = 2" |
      "A <> 1" !! "A <> 1" |
      "A <> 1" !! "A < 1" |
      "A <> 1" !! "A < 0" |
      "A <> 1" !! "A <= 0" |
      "A <> 1" !! "A > 1" |
      "A <> 1" !! "A > 2" |
      "A <> 1" !! "A >= 2" |
      "A < 1" !! "A = 0" |
      "A < 1" !! "A < 1" |
      "A < 1" !! "A <= 0" |
      "A <= 1" !! "A = 1" |
      "A <= 1" !! "A = 0" |
      "A <= 1" !! "A < 1" |
      "A <= 1" !! "A < 0" |
      "A <= 1" !! "A <= 1" |
      "A <= 1" !! "A <= 0" |
      "A > 1" !! "A = 2" |
      "A > 1" !! "A > 1" |
      "A > 1" !! "A > 2" |
      "A > 1" !! "A >= 2" |
      "A >= 1" !! "A = 1" |
      "A >= 1" !! "A = 2" |
      "A >= 1" !! "A > 1" |
      "A >= 1" !! "A > 2" |
      "A >= 1" !! "A >= 1" |
      "A >= 1" !! "A >= 2" |
      "A IS NULL" !! "A IS NULL" |
      "A IS NOT NULL" !! "A = 1" |
      "A IS NOT NULL" !! "A <> 1" |> {
        (lhs, rhs) => f(lhs) must haveAsSubset(f(rhs))
      }
    }

  val intersectionTests =
    "Verify finding intersections of filters" ! {
      "LHS" || "RHS" || "Result" |
      // "A >= 1" !! "A <= 1" !! "A = 1"|
      "A < 1 OR A IS NULL" !! "A IS NOT NULL" !! "A < 1" |
      "A < 1 OR A IS NULL" !! "A IS NOT NULL" !! "A < 1"|
      "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL"|
      "A < 1 OR A IS NULL" !! "A <> 1 OR A IS NULL" !! "A < 1 OR A IS NULL"|
      "A < 1" !! "A > 1" !! "EXCLUDE" |
      "A <= 1" !! "A > 1" !! "EXCLUDE" |
      "A >= 1" !! "A < 1" !! "EXCLUDE" |
      "A = 1" !! "A > 1" !! "EXCLUDE" |
      "A > 1" !! "A = 1" !! "EXCLUDE" |
      "A = 1" !! "A <> 1" !! "EXCLUDE" |
      "A > 2" !! "A >= 2" !! "A > 2"|
      "A < 2" !! "A <= 2" !! "A < 2"|
      "A < 1" !! "A <> 1" !! "A < 1"|
      "A <> 1" !! "A < 1" !! "A < 1"|
      "A < 1" !! "A < 1" !! "A < 1"|
      "A < 2" !! "A < 1" !! "A < 1"|
      "A <= 1" !! "A < 1" !! "A < 1"|
      "A LIKE 'abc%'" !! "A NOT LIKE 'abc%'" !! "EXCLUDE" |
      "A > 2 AND A < 4" !! "A > 4" !! "EXCLUDE" |
      "A IS NULL" !! "A IS NOT NULL" !! "EXCLUDE" |
      "A > 2" !! "A IS NOT NULL" !! "A > 2"|
      "A IS NOT NULL" !! "A > 2" !! "A > 2"|
      "A > 2" !! "A IS NULL OR A < 10" !! "A > 2 AND A < 10"|> {
        (lhs, rhs, expected) =>
          intersection(f(lhs), f(rhs)) must (
            (beSome[Filter]) and
            (beEquivalentTo(f(expected)) ^^ { (_: Option[Filter]).get })
          )
      }
    }
    
  val constraintTests =
    "constraint tests" ! {
      "LHS" || "RHS" || "Expected" |
      "A > 2 OR A < 4" !! "A > 4" !! "A > 4" |
      "PERSONS >= 4000000" !!"PERSONS < 4000000" !! "EXCLUDE" |
      "A = 'bar'" !! "B = 'foo' or A = 'bar'" !! "A = 'bar'" |> {
        (lhs, rhs, expected) =>
          constrain(f(lhs), f(rhs)) must beEquivalentTo(f(expected))
      }
    }

  val unionTests =
    "union tests" ! {
      "LHS"              || "RHS"    || "Expected"        |
      "A < 4"            !! "A > 1"  !! "INCLUDE"         |
      "A < 4"            !! "A > 1"  !! "INCLUDE"         |
      "A = 1"            !! "A <> 1" !! "INCLUDE"         |
      "A >= 1"           !! "A < 1"  !! "INCLUDE"         |
      "A <= 1"           !! "A > 1"  !! "INCLUDE"         |
      "A < 1"            !! "A <> 1" !! "A <> 1"          |
      "A <> 1"           !! "A < 1"  !! "A <> 1"          |
      "A < 1"            !! "A < 1"  !! "A < 1"           |
      "A < 2"            !! "A < 1"  !! "A < 2"           |
      "A <= 1"           !! "A < 1"  !! "A <= 1"          |
      "A > 2 AND A <= 4" !! "A > 4"  !! "A > 2"           |
      "A <= 2 OR A > 4"  !! "A > 4"  !! "A <= 2 OR A > 4" |> {
        (lhs, rhs, expected) =>
          union(f(lhs), f(rhs)) must (
            (beSome[Filter]) and
            (beEquivalentTo(f(expected)) ^^ { (_: Option[Filter]).get })
          )
      }
    }

// pending:
//   "A > 1" !! "A = 1" !! "A >= 1" |
//   "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL" |
//   "A < 1 OR A IS NULL" !! "A <> 1 OR A IS NULL" !! "A <> 1 OR A IS NULL" |
//   "A LIKE 'abc%'" !! "A NOT LIKE 'abc%'" !! "INCLUDE" |

//    // TODO: this one needs working simplification too
//    // relax(f("A > 2 OR A < 4"), f("A > 4")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("PERSONS >= 4000000"), f("PERSONS < 4000000")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A = 'bar'"), f("B = 'foo' OR A = 'bar'")) must beEquivalentTo(f("A = 'bar' OR B = 'foo'"))
//  

  val simplifierTests =
    "Verify simplifier functionality" ! {
      "Input" || "Simplified" | 
      "(A > 1 AND A < 3) OR (B > 1 AND B < 3)" !! "(A > 1 AND A < 3) OR (B > 1 AND B < 3)" |
      // "(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)" !! "(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)" |
      // "(A <> 2 OR A IS NULL) AND (A < 1 OR A IS NULL) AND A IS NOT NULL" !! "A < 1" |
      "A <> 1" !! "A IS NULL OR A <> 1" |
      "A >= 1 OR A IS NULL OR A = 2" !! "A >= 1 OR A IS NULL" |
      "A = 1 AND (B = 2 OR A = 1)" !! "A = 1" |
      "A = 1 AND B = 2 AND A = 1" !! "A = 1 AND B = 2" |
      "A < 1 AND A < 1" !! "A < 1" |
      "A > 2 AND A < 1" !! "EXCLUDE" |
      "A < 1 AND A > 2" !! "EXCLUDE" |
      "A IS NULL AND A IS NOT NULL" !! "EXCLUDE" |
      "A <> 2 AND A IS NOT NULL" !! "A <> 2" |
      "(A <> 2 OR A IS NULL) AND A IS NOT NULL" !! "A <> 2" |
      "A < 1 AND A <> 2" !! "A < 1" |
      "A <= 1 OR A IS NULL OR A <= 1 OR A IS NULL" !! "A <=1 OR A IS NULL" |
      "A > 15 AND (A < 20 OR A IS NULL)" !! "A > 15 AND A < 20" |> {
        (input, expected) => simplify(f(input)) must beEquivalentTo(f(expected))
      }
    }

  def beEquivalentTo(a: Filter): matcher.Matcher[Filter] =
    new matcher.Matcher[Filter] {
      override def apply[F <: Filter](exp: matcher.Expectable[F])
      : matcher.MatchResult[F] =
        result(
          equivalent(a, exp.value),
          "%s equivalent to %s" format(exp.description, a),
          "%s was not equivalent to %s" format(exp.description, a),
          exp
        )
    }

  def haveAsSubset(a: Filter): matcher.Matcher[Filter] =
    new matcher.Matcher[Filter] {
      override def apply[F <: Filter](exp: matcher.Expectable[F])
      : matcher.MatchResult[F] =
        result(
          isSubSet(exp.value, a),
          "%s has subset %s" format(exp.description, a),
          "%s is not superset of %s" format(exp.description, a),
          exp
        )
    }

  import org.geoscript.support.logic.Knowledge.{ Absurdity, Oblivion }
  val kb = Oblivion[Filter]
  import kb.{ reduce, given }

  def equivalent(f: Filter, g: Filter): Boolean = 
    (reduce(f) == reduce(g)) ||
    (given(f).reduce(g) == Filter.INCLUDE) &&
    (given(g).reduce(f) == Filter.INCLUDE)

  def isSubSet(f: Filter, g: Filter): Boolean =
    reduce(g) == Filter.EXCLUDE ||
    given(g) != Absurdity &&
    given(g).reduce(f) == Filter.INCLUDE

  def allOf(fs: Seq[Filter]): Filter =
    fs match {
      case Seq() => Filter.INCLUDE
      case Seq(f) => f
      case fs => filters.and(fs)
    }

  val simplify = kb.reduce _
  val negate = FiltersAreSentential.not _

  def constrain(f: Filter, g: Filter): Filter = {
    val and = FiltersAreSentential.and(f, g)
    reduce(and)
  }

  def intersection(f: Filter, g: Filter): Option[Filter] = {
    val and = FiltersAreSentential.and(f, g)
    Option(reduce(and)).filter(and !=)
  }

  def union(f: Filter, g: Filter): Option[Filter] = {
    val or = FiltersAreSentential.or(f, g)
    Some(reduce(or)).filter(or !=)
  }
}
