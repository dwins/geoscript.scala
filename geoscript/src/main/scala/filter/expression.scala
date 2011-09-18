package org.geoscript.filter
import org.opengis.filter.{expression => ogc}

sealed abstract trait Expression {
  def underlying: ogc.Expression
}

object Expression {
  case class Wrapped(underlying: ogc.Expression)
  extends Expression

  private val filter =
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)

  implicit def enrichString(s: String) =
  new {
    def cql: Expression =
      Wrapped(org.geotools.filter.text.ecql.ECQL.toExpression(s))
  }

  implicit def literalToExpression(x: String): Expression =
  Wrapped(filter.literal(x))

  implicit def literalToExpression(x: Double): Expression =
  Wrapped(filter.literal(x))

  implicit def unwrap(wrapped: Expression): ogc.Expression = 
    wrapped.underlying

  implicit def wrap(underlying: ogc.Expression): Expression =
    Wrapped(underlying)
}
