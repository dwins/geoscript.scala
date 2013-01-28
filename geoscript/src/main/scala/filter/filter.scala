package org.geoscript
package object filter {
  type Filter = org.opengis.filter.Filter
  type Expression = org.opengis.filter.expression.Expression
  val Include = org.opengis.filter.Filter.INCLUDE
  private val factory = org.geotools.factory.CommonFactoryFinder.getFilterFactory()

  def literal(x: Double): Expression = factory.literal(x)
}
