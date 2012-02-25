package org.geoscript.geocss

import SelectorOps._
import org.geotools.filter.text.ecql.ECQL

import org.specs2._, org.specs2.matcher._

class SelectorTest extends Specification with matcher.DataTables {
  def in(s: String) = getClass.getResourceAsStream(s)
  def is = implication

  val implication =
    "Selector implication" ! {
      "lhs"     || "rhs"    |
      "INCLUDE" !! "A <= 2" |
      "A <= 4"  !! "A <= 2" |
      "A >= 2"  !! "A >= 4" |> { (lhs, rhs) =>
        ExpressionSelector(lhs) must imply(ExpressionSelector(rhs))
      }
    } ^ 
    "Selector implication (negative tests)" ! (
      "lhs"     || "rhs"     |
      "A = 1"   !! "INCLUDE" |
      "A >= 4"  !! "A >= 2"  |> { (lhs, rhs) =>
        ExpressionSelector(lhs) must not(imply(ExpressionSelector(rhs)))
      }
    ) ^
    "Selector implication ('exclude' is treated specially)" ! 
      { ExpressionSelector("EXCLUDE") must (
          imply(ExpressionSelector("EXCLUDE")) and
          beEquivalentTo(ExpressionSelector("EXCLUDE"))
        )
      }

  val simplification =
    "we should be able to simplify selectors" ! {
      val any: Selector = AcceptSelector
      val id : Selector = IdSelector("states.9")
      val cql: Selector = ExpressionSelector("STATE_NAME LIKE '%ia'")
  
      "to simplify"               | "expected"    |
      List(id)                    ! List(id)      |
      List(any, id)               ! List(id)      |
      List(any, cql, id)          ! List(cql, id) |
      List(cql, id)               ! List(cql, id) |
      List(cql, NotSelector(cql)) ! List(Exclude) |
      List(ExpressionSelector("A >= 2"),
           ExpressionSelector("A < 4")) ! List(ExpressionSelector("A >= 2"),
                                               ExpressionSelector("A < 4"))  |
      List(ExpressionSelector("A >= 2"),
           ExpressionSelector("A < 2")) ! List(Exclude) |
      List(ExpressionSelector("A >= 2"),
           ExpressionSelector("A >= 4")) ! List(ExpressionSelector("A < 4")) |
      List(ExpressionSelector("A < 2"),
           ExpressionSelector("A >= 2"),
           ExpressionSelector("A < 4"),
           ExpressionSelector("A >= 4"))              ! List(Exclude) |
      List(WrappedFilter(ECQL.toFilter("PERSONS>=4")),
           ExpressionSelector("PERSONS < 4"),
           ExpressionSelector("PERSONS > 2"))         ! List(Exclude) |
      Nil                         ! List(Exclude) |> { (input, expected) =>
        simplify(input) must containAllOf(expected).only
      }
    }

   def beEquivalentTo(s: Selector): Matcher[Selector] =
     new Matcher[Selector] {
       def apply[S <: Selector](t: Expectable[S]) = 
         result(
           homologous(t.value, s),
           "%s implies %s" format(t.value, s),
           "%s does not imply %s" format(t.value, s),
           t
         )
     }

   def imply(s: Selector): Matcher[Selector] =
     new Matcher[Selector] {
       def apply[S <: Selector](t: Expectable[S]) = 
         result(
           isSubSet(t.value, s),
           "%s implies %s" format(t.value, s),
           "%s does not imply %s" format(t.value, s),
           t
         )
     }
}
