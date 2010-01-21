package org.geoscript.geometry

import com.vividsolutions.jts.{geom => jts}

object Ring {
  def apply(coords: jts.Coordinate*) =
    ModuleInternals.factory.createLinearRing(coords.toArray)
}
