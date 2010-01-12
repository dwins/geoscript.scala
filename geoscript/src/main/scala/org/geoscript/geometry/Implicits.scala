package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}

object Implicits {
  implicit def enrich(p: jts.Point): RichPoint = new RichPoint(p)
  implicit def enrich(g: jts.Geometry): RichGeometry = new RichGeometry(g)
}
