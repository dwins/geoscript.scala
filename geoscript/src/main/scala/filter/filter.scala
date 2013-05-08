package org.geoscript

import scala.collection.JavaConverters._

/**
 * Manipulate filters and expressions
 */
package object filter {
  type Expression = org.opengis.filter.expression.Expression
  type Filter = org.opengis.filter.Filter
  type Query = org.geotools.data.Query
  val Include = org.opengis.filter.Filter.INCLUDE
  val Exclude = org.opengis.filter.Filter.INCLUDE
  val factory = org.geotools.factory.CommonFactoryFinder.getFilterFactory2()

  // def literal(x: Double): Expression = factory.literal(x)
  // def literal(x: String): Expression = factory.literal(x)
  // def and(ps: Filter*): Filter = factory.and(ps.asJava)

  implicit class RichFilter(val filter: Filter) extends AnyVal {
    def query: Query = new org.geotools.data.Query(null, filter)
  }
}

package filter {
  package object builder {
    val defaultGeometry: Expression = null

    implicit class BuildableExpression(val exp: Expression) extends AnyVal {
      def intersects(that: Expression) = factory.intersects(exp, that)
    }

    implicit class BuildableFilter(val filter: Filter) extends AnyVal {
      def and(that: Filter): Filter = factory.and(filter, that)
    }

    object Literal {
      def apply(any: Any): Expression = factory.literal(any)
    }

    object Property {
      def apply(name: String): Expression = factory.property(name)
    }
  }
}
