package org.geoscript.geocss.filter

import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL

import org.specs2._

class FilterTest extends Specification {
  import FilterOps._
  import ECQL.{ toFilter => f }

  // case class beEquivalentTo(a: Filter) extends matcher.Matcher[Filter] {
  //   def apply(v: => Filter) = {
  //     val prefix = a.toString + " and " + v.toString
  //     (equivalent(a, v), prefix + " were equivalent", prefix + " were not equivalent")
  //   }
  // }

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

  def is =
    "basic filter equivalence tests should work" ^
      equivalenceTests ^ end ^
    "filter negation should be aware of binary operations" ^
      negationTests ^ end ^
    "negated binary operators are complicated" ^
      binaryOperatorSimplificationTests ^ end ^
    "subset tests should be aware of binary operations" ^ 
      subsetTests ^ end

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
      isSubSet(f("A <> 2"), f("A < 2")) must beTrue } ^ 
    "A < 1 is a subset of A <> 2" ! {
      isSubSet(f("A <> 2"), f("A < 1")) must beTrue } ^ 
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
  
//  "redundant binary operations should be detected" in {
//    import ECQL.{ toFilter => f }
//
//    isSubSet(f("A = 1"), f("A = 1")) must beTrue
//
//    isSubSet(f("A = 1"), f("A <> 1")) must beFalse
//
//    isSubSet(f("A = 1"), f("A < 1")) must beFalse
//
//    isSubSet(f("A = 1"), f("A <= 1")) must beFalse
//
//    isSubSet(f("A = 1"), f("A > 1")) must beFalse
//
//    isSubSet(f("A = 1"), f("A >= 1")) must beFalse
//
//    isSubSet(f("A <> 1"), f("A = 1")) must beFalse
//    isSubSet(f("A <> 1"), f("A = 2")) must beTrue
//
//    isSubSet(f("A <> 1"), f("A <> 1")) must beTrue
//
//    isSubSet(f("A <> 1"), f("A < 1")) must beTrue
//    isSubSet(f("A <> 1"), f("A < 0")) must beTrue
//    isSubSet(f("A <> 1"), f("A < 2")) must beFalse
//
//    isSubSet(f("A <> 1"), f("A <= 1")) must beFalse
//    isSubSet(f("A <> 1"), f("A <= 0")) must beTrue
//    isSubSet(f("A <> 1"), f("A <= 2")) must beFalse
//
//    isSubSet(f("A <> 1"), f("A > 1")) must beTrue
//    isSubSet(f("A <> 1"), f("A > 0")) must beFalse
//    isSubSet(f("A <> 1"), f("A > 2")) must beTrue
//
//    isSubSet(f("A <> 1"), f("A >= 1")) must beFalse
//    isSubSet(f("A <> 1"), f("A >= 0")) must beFalse
//    isSubSet(f("A <> 1"), f("A >= 2")) must beTrue
//
//    isSubSet(f("A < 1"), f("A = 1")) must beFalse
//    isSubSet(f("A < 1"), f("A = 0")) must beTrue
//
//    isSubSet(f("A < 1"), f("A <> 1")) must beFalse
//    isSubSet(f("A < 1"), f("A <> 0")) must beFalse
//    isSubSet(f("A < 1"), f("A <> 2")) must beFalse
//
//    isSubSet(f("A < 1"), f("A < 1")) must beTrue
//
//    isSubSet(f("A < 1"), f("A <= 1")) must beFalse
//    isSubSet(f("A < 1"), f("A <= 0")) must beTrue
//    isSubSet(f("A < 1"), f("A <= 2")) must beFalse
//
//    isSubSet(f("A < 1"), f("A > 1")) must beFalse
//
//    isSubSet(f("A < 1"), f("A >= 1")) must beFalse
//
//    isSubSet(f("A <= 1"), f("A = 1")) must beTrue
//    isSubSet(f("A <= 1"), f("A = 0")) must beTrue
//    isSubSet(f("A <= 1"), f("A = 2")) must beFalse
//
//    isSubSet(f("A <= 1"), f("A <> 1")) must beFalse
//    isSubSet(f("A <= 1"), f("A <> 0")) must beFalse
//    isSubSet(f("A <= 1"), f("A <> 2")) must beFalse
//
//    isSubSet(f("A <= 1"), f("A < 1")) must beTrue
//    isSubSet(f("A <= 1"), f("A < 0")) must beTrue
//    isSubSet(f("A <= 1"), f("A < 2")) must beFalse
//
//    isSubSet(f("A <= 1"), f("A <= 1")) must beTrue
//    isSubSet(f("A <= 1"), f("A <= 0")) must beTrue
//    isSubSet(f("A <= 1"), f("A <= 2")) must beFalse
//
//    isSubSet(f("A <= 1"), f("A > 1")) must beFalse
//
//    isSubSet(f("A <= 1"), f("A >= 1")) must beFalse
//
//    isSubSet(f("A > 1"), f("A = 1")) must beFalse
//    isSubSet(f("A > 1"), f("A = 0")) must beFalse
//    isSubSet(f("A > 1"), f("A = 2")) must beTrue
//
//    isSubSet(f("A > 1"), f("A <> 1")) must beFalse
//
//    isSubSet(f("A > 1"), f("A < 1")) must beFalse
//
//    isSubSet(f("A > 1"), f("A <= 1")) must beFalse
//
//    isSubSet(f("A > 1"), f("A > 1")) must beTrue
//    isSubSet(f("A > 1"), f("A > 0")) must beFalse
//    isSubSet(f("A > 1"), f("A > 2")) must beTrue
//
//    isSubSet(f("A > 1"), f("A >= 1")) must beFalse
//    isSubSet(f("A > 1"), f("A >= 0")) must beFalse
//    isSubSet(f("A > 1"), f("A >= 2")) must beTrue
//
//    isSubSet(f("A >= 1"), f("A = 1")) must beTrue
//    isSubSet(f("A >= 1"), f("A = 0")) must beFalse
//    isSubSet(f("A >= 1"), f("A = 2")) must beTrue
//
//    isSubSet(f("A >= 1"), f("A <> 1")) must beFalse
//    isSubSet(f("A >= 1"), f("A <> 0")) must beFalse
//    isSubSet(f("A >= 1"), f("A <> 2")) must beFalse
//
//    isSubSet(f("A >= 1"), f("A < 1")) must beFalse
//
//    isSubSet(f("A >= 1"), f("A <= 1")) must beFalse
//
//    isSubSet(f("A >= 1"), f("A > 1")) must beTrue
//    isSubSet(f("A >= 1"), f("A > 0")) must beFalse
//    isSubSet(f("A >= 1"), f("A > 2")) must beTrue
//
//    isSubSet(f("A >= 1"), f("A >= 1")) must beTrue
//    isSubSet(f("A >= 1"), f("A >= 0")) must beFalse
//    isSubSet(f("A >= 1"), f("A >= 2")) must beTrue
//
//    isSubSet(f("A IS NULL"), f("A IS NULL")) must beTrue
//    isSubSet(f("A IS NULL"), f("A IS NOT NULL")) must beFalse
//    isSubSet(f("A IS NOT NULL"), f("A IS NULL")) must beFalse
//    isSubSet(f("A IS NOT NULL"), f("A = 1")) must beTrue
//    isDisjoint(f("A IS NOT NULL"), f("A <> 1")) must beFalse
//    isSubSet(f("A IS NOT NULL"), f("A <> 1")) must beTrue
//  }
//
//  "constraining filters should be aware of binary operations" in {
//    import ECQL.{ toFilter => f }
//    intersection(f("A < 1 OR A IS NULL"), f("A IS NOT NULL")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1")) }
//    intersection(f("A < 1 OR A IS NULL"), f("A < 1 OR A IS NULL")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1 OR A IS NULL")) }
//    intersection(f("A < 1 OR A IS NULL"), f("A <> 1 OR A IS NULL")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1 OR A IS NULL")) }
//    intersection(f("A < 1"), f("A > 1")) must beSome(Filter.EXCLUDE)
//    intersection(f("A <= 1"), f("A > 1")) must beSome(Filter.EXCLUDE)
//    intersection(f("A >= 1"), f("A < 1")) must beSome(Filter.EXCLUDE)
//    intersection(f("A >= 1"), f("A <= 1")) must beSome[Filter].which {
//      _ must beEquivalentTo(f("A = 1"))
//    }
//    intersection(f("A = 1"), f("A > 1")) must beSome(Filter.EXCLUDE)
//    intersection(f("A > 1"), f("A = 1")) must beSome(Filter.EXCLUDE)
//    intersection(f("A = 1"), f("A <> 1")) must beSome(Filter.EXCLUDE)
//    intersection(f("A > 2"), f("A >= 2")) must beSome[Filter].which {
//      _ must beEquivalentTo(f("A > 2"))
//    }
//    intersection(f("A < 2"), f("A <= 2")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 2")) }
//    intersection(f("A < 1"), f("A <> 1")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1")) }
//    intersection(f("A <> 1"), f("A < 1")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1")) }
//    intersection(f("A < 1"), f("A < 1")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1")) }
//    intersection(f("A < 2"), f("A < 1")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1")) }
//    intersection(f("A <= 1"), f("A < 1")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A < 1")) }
//    intersection(f("A LIKE 'abc%'"), f("A NOT LIKE 'abc%'")) must beSome(Filter.EXCLUDE)
//    intersection(f("A > 2 AND A < 4"), f("A > 4")) must beSome(Filter.EXCLUDE)
//    intersection(f("A IS NULL"), f("A IS NOT NULL")) must beSome(Filter.EXCLUDE)
//    intersection(f("A > 2"), f("A IS NOT NULL")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A > 2")) }
//    intersection(f("A IS NOT NULL"), f("A > 2")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A > 2")) }
//    intersection(f("A > 2"), f("A IS NULL OR A < 10")) must
//      beSome[Filter].which { _ must beEquivalentTo(f("A > 2 AND A < 10")) }
//    // constrain(f("A > 2 OR A < 4"), f("A > 4")) must beEquivalentTo(f("A > 4"))
//    // constrain(f("PERSONS >= 4000000"), f("PERSONS < 4000000")) must beEquivalentTo(f("EXCLUDE"))
//    // constrain(f("A = 'bar'"), f("B = 'foo' or A = 'bar'")) must beEquivalentTo(f("A = 'bar'"))
//  }
//
//  "unioning filters should be aware of binary operations" in {
//    import ECQL.{ toFilter => f }
//    relax(f("A <= 1"), f("A > 1")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A < 1"), f("A <> 1")) must beEquivalentTo(f("A <> 1"))
//    relax(f("A <> 1"), f("A < 1")) must beEquivalentTo(f("A <> 1"))
//    relax(f("A >= 1"), f("A < 1")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A > 1"), f("A = 1")) must beEquivalentTo(f("A >= 1"))
//    relax(f("A = 1"), f("A <> 1")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A < 1"), f("A < 1")) must beEquivalentTo(f("A < 1"))
//    relax(f("A < 2"), f("A < 1")) must beEquivalentTo(f("A < 2"))
//    relax(f("A < 4"), f("A > 1")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A <= 1"), f("A < 1")) must beEquivalentTo(f("A <= 1"))
//    relax(f("A < 1 OR A IS NULL"), f("A < 1 OR A IS NULL")) must beEquivalentTo(f("A < 1 OR A IS NULL"))
//    relax(f("A < 1 OR A IS NULL"), f("A <> 1 OR A IS NULL")) must beEquivalentTo(f("A <> 1 OR A IS NULL"))
//    relax(f("A LIKE 'abc%'"), f("A NOT LIKE 'abc%'")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A > 2 AND A <= 4"), f("A > 4")) must beEquivalentTo(f("A > 2"))
//    relax(f("A <= 2 OR A > 4"), f("A > 4")) must beEquivalentTo(f("A <= 2 OR A > 4"))
// 
//    // TODO: this one needs working simplification too
//    // relax(f("A > 2 OR A < 4"), f("A > 4")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("PERSONS >= 4000000"), f("PERSONS < 4000000")) must beEquivalentTo(f("INCLUDE"))
//    relax(f("A = 'bar'"), f("B = 'foo' OR A = 'bar'")) must beEquivalentTo(f("A = 'bar' OR B = 'foo'"))
//  }
//
//  "it should be possible to simplify filters" in {
//    import ECQL.{ toFilter => f }
//    // // comment out the assertions for now, but we still want to at least check
//    // // that these terminate without a StackOverflowError
//    simplify(f("(A > 1 AND A < 3) OR (B > 1 AND B < 3)")) // must beEquivalentTo(f("(A > 1 AND A < 3) OR (B > 1 AND B < 3)"))
//    simplify(f("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)")) // must beEquivalentTo(f("(A <= 1 OR A IS NULL) AND (B >= 2 OR B IS NULL)"))
//
//    // things that actually work now
//    simplify(f("A >= 1 OR A IS NULL OR A = 2")) must beEquivalentTo(f("A >= 1 OR A IS NULL"))
//    simplify(f("A <> 1")) must 
//      beEquivalentTo(
//        filters.or(
//          filters.isNull(filters.property("A")),
//          filters.notEqual(filters.property("A"), filters.literal(1L))
//        )
//      )
//    simplify(f("A = 1 AND (B = 2 OR A = 1)")) must beEquivalentTo(f("A = 1"))
//    simplify(f("A = 1 AND B = 2 AND A = 1")) must beEquivalentTo(f("A = 1 AND B = 2"))
//    simplify(f("A < 1 AND A < 1")) must beEquivalentTo(f("A < 1"))
//    simplify(f("A > 2 AND A < 1")) must beEquivalentTo(f("EXCLUDE"))
//    simplify(f("A < 1 AND A > 2")) must beEquivalentTo(f("EXCLUDE"))
//    simplify(f("A IS NULL AND A IS NOT NULL")) must beEquivalentTo(f("EXCLUDE"))
//    simplify(f("A <> 2 AND A IS NOT NULL")) must beEquivalentTo(f("A <> 2"))
//    simplify(f("(A <> 2 OR A IS NULL) AND A IS NOT NULL")) must beEquivalentTo(f("A <> 2"))
//    simplify(f("NOT A < 1")) must beEquivalentTo(f("A >= 1 OR A IS NULL"))
//    simplify(f("A < 1 AND A <> 2")) must beEquivalentTo(f("A < 1"))
//    simplify(f("(A <> 2 OR A IS NULL) AND (A < 1 OR A IS NULL) AND A IS NOT NULL")) must beEquivalentTo(f("A < 1"))
//    simplify(f("A <= 1 OR A IS NULL OR A <= 1 OR A IS NULL")) must beEquivalentTo(f("A <=1 OR A IS NULL"))
//    simplify(f("A > 15 AND (A < 20 OR A IS NULL)")) must 
//      beEquivalentTo(f("A > 15 AND A < 20"))
//  }
}
