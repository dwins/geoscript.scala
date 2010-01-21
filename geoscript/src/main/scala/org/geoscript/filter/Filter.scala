package org.geoscript.filter

import com.vividsolutions.jts.{geom=>jts}
import org.{geotools => gt}

import org.geoscript.geometry.Geometry

object Intersects {
  def apply(g: Geometry) = {
    val ff = gt.factory.CommonFactoryFinder.getFilterFactory2(null)
    ff.intersects(null, ff.literal(g.underlying))
  }
}
