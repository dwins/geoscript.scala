package org.geoscript

import scala.collection.JavaConverters._

/**
 * Manipulate filters and expressions
 */
package object filter {
  type Filter = org.opengis.filter.Filter
  type Expression = org.opengis.filter.expression.Expression
  val Include = org.opengis.filter.Filter.INCLUDE
  val Exclude = org.opengis.filter.Filter.INCLUDE
  val factory = org.geotools.factory.CommonFactoryFinder.getFilterFactory2()

  def literal(x: Double): Expression = factory.literal(x)
  def literal(x: String): Expression = factory.literal(x)
  def and(ps: Filter*): Filter = factory.and(ps.asJava)
}
