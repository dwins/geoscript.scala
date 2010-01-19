package org.geoscript.filter

import com.vividsolutions.jts.{geom=>jts}
import org.{geotools => gt}

object Intersects {
  def apply(g: jts.Geometry) = {
    val ff = gt.factory.CommonFactoryFinder.getFilterFactory2(null)
    ff.intersects(null, ff.literal(g))
  }
}
