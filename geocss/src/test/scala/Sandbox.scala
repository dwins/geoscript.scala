package org.geoscript.geocss

import org.geoscript.support.logic.reduce

object Sandbox extends App {
  import org.geotools.filter.text.ecql.ECQL.toFilter
  import Selector.asSelector
  val cql = (toFilter(_: String)) andThen (asSelector _)

  val oriole = Seq(Or(Seq(And(Seq(cql("species = 'oriole'"))))))
  val robin = Seq(Or(Seq(And(Seq(cql("species = 'robin'"))))))
  println(reduce[Selector](And(oriole ++ robin)) == Exclude)
}
