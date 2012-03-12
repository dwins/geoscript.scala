package org.geoscript.geocss

import SelectorOps._
import org.geotools.filter.text.ecql.ECQL

import org.specs2._, org.specs2.matcher._

class SelectorTest extends Specification with matcher.DataTables {
  import SelectorsAreSentential.{ disprovenBy, provenBy }
  val kb = dwins.logic.Knowledge.Oblivion(SelectorsAreSentential)

  def scale_<(s: String): Selector = PseudoSelector("scale", "<", s)
  def scale_>(s: String): Selector = PseudoSelector("scale", ">", s)
  def not(s: Selector): Selector = Not(s)
  def cql(s: String): Selector = ExpressionSelector(s)

  def is = 
    "disproven test" ! {
      "Givens"          | "Query"           | "Disproven?" |
      Set(scale_>("1")) ! scale_<("0")      ! true         |
      Set(scale_>("1")) ! not(scale_>("0")) ! true         |
      Set(not(scale_>("0"))) ! scale_>("1") ! true         |
      Set(scale_>("1")) ! scale_>("0")      ! false        |
      Set(cql("A=1"))   ! cql("A = 2")      ! true         |
      Set(cql("A=2"))   ! cql("A = 2")      ! false        |> {
        (known, query, expected) => disprovenBy(known, query) must_== expected
      }
    } ^
    "proven test" ! {
      "Givens"          | "Query"      | "Proven?" |
      Set(cql("A=1"))   ! cql("A=2") ! false     |
      Set(cql("A=2"))   ! cql("A=2") ! true      |
      Set(scale_>("1")) ! scale_>("0") ! true      |> {
        (known, query, expected) => provenBy(known, query) must_== expected
      }
    }
}
