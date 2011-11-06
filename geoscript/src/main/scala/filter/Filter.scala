package org.geoscript.filter

import com.vividsolutions.jts.{geom=>jts}
import org.{geotools => gt}
import org.opengis.{filter => ogc}

import org.geoscript.geometry.Geometry

trait Filter {
  def underlying: ogc.Filter
}

object Filter {
  private val factory = gt.factory.CommonFactoryFinder.getFilterFactory2(null)

  private class Wrapper(val underlying: ogc.Filter) extends Filter

  object Include extends Filter {
    val underlying = ogc.Filter.INCLUDE
    def unapply(f: Filter): Boolean = f.underlying == ogc.Filter.INCLUDE
  }

  def intersects(geometry: Geometry): Filter = {
    new Wrapper(
      factory.intersects(null, factory.literal(geometry))
    )
  }

  def id(ids: Seq[String]): Filter = {
    val idSet = new java.util.HashSet[ogc.identity.Identifier]()
    for (i <- ids) { idSet.add(factory.featureId(i)) }
    new Wrapper(factory.id(idSet))
  }

  def or(filters: Seq[Filter]): Filter = {
    val idList = new java.util.ArrayList[ogc.Filter]()
    for (f <- filters) { idList.add(f.underlying) }
    new Wrapper(factory.or(idList))
  }

  implicit def wrap(f: ogc.Filter): Filter = new Wrapper(f)
  implicit def unwrapped(f: Filter): ogc.Filter = f.underlying
}
