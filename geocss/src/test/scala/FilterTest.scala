package org.geoscript.geocss.filter

import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL

import org.specs2._

class FilterTest extends Specification with matcher.DataTables {
  import FilterOps._
  import ECQL.{ toFilter => f }

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

  def is =
    "basic filter equivalence tests should work" ^
      equivalenceTests ^ end ^
    "filter negation should be aware of binary operations" ^
      negationTests ^ end ^
    "negated binary operators are complicated" ^
      binaryOperatorSimplificationTests ^ end ^
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
    "exclude === exclude" ! {
      f("EXCLUDE") must beEquivalentTo(f("EXCLUDE")) 
    } ^
    "include === include" ! {
      f("INCLUDE") must beEquivalentTo(f("INCLUDE"))
    } ^
    "A < 1 === A < 1" ! {
      f("A < 1") must beEquivalentTo(f("A < 1"))
    } ^
    "A <> 1 === A <> 1" ! {
      f("A <> 1") must beEquivalentTo(f("A <> 1"))
    } ^
    "A IS NULL === A IS NULL" ! {
      f("A IS NULL") must beEquivalentTo(f("A IS NULL"))
    } ^
    "A <= 1 OR B >= 1 === A <= 1 OR B >= 2" ! {
      f("A <= 1 OR B >= 2") must beEquivalentTo(f("A <= 1 OR B >= 2"))
    } ^
    "A <= 1 AND B >= 2 === A <= 1 AND B >= 2" ! {
      f("A <= 1 AND B >= 2") must beEquivalentTo(f("A <= 1 AND B >= 2"))
    } ^
    "A < 1 !=== A > 1" !  {
      f("A < 1") must not(beEquivalentTo(f("A > 1")))
    } ^
    "A < 1 !=== A = 1" ! {
      f("A < 1") must not(beEquivalentTo(f("A = 1")))
    } ^
    "A = 1 !=== A > 1" ! {
      f("A = 1") must not(beEquivalentTo(f("A > 1")))
    }

  val negationTests = 
    "NOT (A < 1) === (A >= 1 OR A IS NULL)" ! {
      negate(f("A < 1")) must beEquivalentTo(f("A >= 1 OR A IS NULL"))
    } ^
    "NOT (A > 1) === (A <= 1 OR A IS NULL)" ! {
      negate(f("A > 1")) must beEquivalentTo(f("A <= 1 OR A IS NULL"))
    } ^
    "NOT (A <= 1) === (A > 1 OR A IS NULL)" ! {
      negate(f("A <= 1")) must beEquivalentTo(f("A > 1 OR A IS NULL"))
    } ^
    "NOT (A >= 1) === (A < 1 OR A IS NULL)" ! {
      negate(f("A >= 1")) must beEquivalentTo(f("A < 1 OR A IS NULL"))
    } ^ {
      pending // ("Need to canonicalize filters before the equality check here works")
      // TODO: negate(f("A > 1 AND B < 2")) must beEquivalentTo(f("A <= 1 OR A IS NULL OR B >= 2 OR B IS NULL"))
      // TODO: figure out and implement a canonical form for nested logical filters
      // negate(f("A > 1 OR B < 2")) must beEquivalentTo(f("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)"))
      // TODO: simplify(negate(f("A < 1 AND A <> 2"))) must beEquivalentTo(f("A IS NULL OR A >= 1"))
      // TODO: simplify(negate(negate(f("A < 1 AND A <> 2")))) must beEquivalentTo(f("A < 1"))
    }


  val binaryOperatorSimplificationTests = 
    "NOT(A < 1) AND (A > 0) === A IS NULL" ! {
      intersection(negate(f("A < 1")), negate(f("A > 0"))) must beSome.which {
        _ must beEquivalentTo(f("A IS NULL"))
      }
    } ^ 
    "NOT(A < 2) AND NOT (A >= 2 OR A <= 4) AND NOT(A > 4) === A IS NULL" ! {
      simplify(allOf(Seq("A < 2", "A >= 2 OR A <= 4", "A > 4") map { s => negate(f(s)) } )) must
        beEquivalentTo(f("A IS NULL"))
    }
   
  val subsetTests = 
    "Anything is a subset of INCLUDE" ! {
      isSubSet(f("INCLUDE"), f("A = 2")) must beTrue
    } ^
    "A = 2 is a subset of A >= 1" ! {
      isSubSet(f("A >= 1"), f("A = 2")) must beTrue
    } ^ 
    "A > 1 is not a subset of A = 1" ! {
      isSubSet(f("A = 1"), f("A > 1")) must beFalse
    } ^ 
    "A = 1 is not a subset of A > 1" ! {
      isSubSet(f("A > 1"), f("A = 1")) must beFalse
    } ^ 
    "A < 1 is a subset of A < 1" ! {
      isSubSet(f("A < 1"), f("A < 1")) must beTrue
    } ^
    "A < 2 is not a subset of A < 1" ! {
      isSubSet(f("A < 1"), f("A < 2")) must beFalse 
    } ^ 
    "A < 1 is a subset of A < 2" ! {
      isSubSet(f("A < 2"), f("A < 1")) must beTrue
    } ^ 
    "A < 1 OR B > 2 is not a subset of A < 1" ! {
      isSubSet(f("A < 1"), f("A < 1 OR B > 2")) must beFalse
    } ^ 
    "A < 0 OR B > 2 is not a subset of A < 1" ! {
      isSubSet(f("A < 1"), f("A < 0 OR B > 2")) must beFalse
    } ^ 
    "A < 1 is a subset of A IS NOT NULL" ! {
      isSubSet(f("A IS NOT NULL"), f("A < 1")) must beTrue
    } ^ 
    "A < 2 is a subset of A <> 2" ! {
      isSubSet(f("A <> 2"), f("A < 2")) must beTrue
    } ^ 
    "A < 1 is a subset of A <> 2" ! {
      isSubSet(f("A <> 2"), f("A < 1")) must beTrue
    } ^ 
    "A < 1 and A <> 2 is a subset of A < 1" ! {
      isSubSet(f("A < 1"), f("A < 1 AND A <> 2")) must beTrue
    } ^ 
    "A < 1 AND A > 0 is a subset of A < 1" ! {
      isSubSet(f("A < 1"), f("A < 1 AND A > 0")) must beTrue
    } ^ 
    "A < 1 is not a subset of A < 1 and A > 0" ! {
      isSubSet(f("A < 1 AND A > 0"), f("A < 1")) must beFalse
    } ^ 
    "A < 1 is a subset of A <> 1" ! {
      isSubSet(f("A <> 1"), f("A < 1")) must beTrue
    } ^
    "A IS NULL is a subset of A <> 1 OR A IS NULL" ! {
      isSubSet(f("A <> 1 OR A IS NULL"), f("A IS NULL")) must beTrue
    } ^ 
    "A = 1 OR A = 2 is a subset of A = 1 OR A = 2" ! {
      isSubSet(f("A = 1 OR A = 2"), f("A = 1 OR A = 2")) must beTrue
    } ^ 
    "A < 1 OR A IS NULL is a subset of A <> 1 OR A is NULL" ! {
      isSubSet(f("A <> 1 OR A IS NULL"), f("A < 1 OR A IS NULL")) must beTrue
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

//    isDisjoint(f("A IS NOT NULL"), f("A <> 1")) must beFalse
// }

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
      "A < 1 OR A IS NULL" !! "A IS NOT NULL" !! "A < 1" |
      "A < 1 OR A IS NULL" !! "A IS NOT NULL" !! "A < 1"|
      "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL"|
      "A < 1 OR A IS NULL" !! "A <> 1 OR A IS NULL" !! "A < 1 OR A IS NULL"|
      "A < 1" !! "A > 1" !! "EXCLUDE" |
      "A <= 1" !! "A > 1" !! "EXCLUDE" |
      "A >= 1" !! "A < 1" !! "EXCLUDE" |
      "A >= 1" !! "A <= 1" !! "A = 1"|
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
          intersection(f(lhs), f(rhs)) must
            (beEquivalentTo(f(expected)) ^^ { (_: Option[Filter]).get })
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
      "LHS" || "RHS" || "Expected" |
      "A <= 1" !! "A > 1" !! "INCLUDE" |
      "A < 1" !! "A <> 1" !! "A <> 1" |
      "A <> 1" !! "A < 1" !! "A <> 1" |
      "A >= 1" !! "A < 1" !! "INCLUDE" |
      "A > 1" !! "A = 1" !! "A >= 1" |
      "A = 1" !! "A <> 1" !! "INCLUDE" |
      "A < 1" !! "A < 1" !! "A < 1" |
      "A < 2" !! "A < 1" !! "A < 2" |
      "A < 4" !! "A > 1" !! "INCLUDE" |
      "A <= 1" !! "A < 1" !! "A <= 1" |
      "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL" !! "A < 1 OR A IS NULL" |
      "A < 1 OR A IS NULL" !! "A <> 1 OR A IS NULL" !! "A <> 1 OR A IS NULL" |
      "A LIKE 'abc%'" !! "A NOT LIKE 'abc%'" !! "INCLUDE" |
      "A > 2 AND A <= 4" !! "A > 4" !! "A > 2" |
      "A <= 2 OR A > 4" !! "A > 4" !! "A <= 2 OR A > 4" |> {
        (lhs, rhs, expected) =>
          union(f(lhs), f(rhs)) must 
            (beEquivalentTo(f(expected)) ^^ { (_: Option[Filter]).get })
      }
    }

//    // TODO: this one needs working simplification too
//    // relax(f("A > 2 OR A < 4"), f("A > 4")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("PERSONS >= 4000000"), f("PERSONS < 4000000")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A = 'bar'"), f("B = 'foo' OR A = 'bar'")) must beEquivalentTo(f("A = 'bar' OR B = 'foo'"))
//  }

  val simplifierTests =
    "Verify simplifier functionality" ! {
      "Input" || "Simplified" | 
      "(A > 1 AND A < 3) OR (B > 1 AND B < 3)" !! "(A > 1 AND A < 3) OR (B > 1 AND B < 3)" |
      // "(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)" !! "(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)" |
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
      "NOT A < 1" !! "A >= 1 OR A IS NULL" |
      "A < 1 AND A <> 2" !! "A < 1" |
      "(A <> 2 OR A IS NULL) AND (A < 1 OR A IS NULL) AND A IS NOT NULL" !! "A < 1" |
      "A <= 1 OR A IS NULL OR A <= 1 OR A IS NULL" !! "A <=1 OR A IS NULL" |
      "A > 15 AND (A < 20 OR A IS NULL)" !! "A > 15 AND A < 20" |> {
        (input, expected) => simplify(f(input)) must beEquivalentTo(f(expected))
      }
    }
}
