package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}

trait Implicits {
  implicit def enrichPoint(p: jts.Point): RichPoint = 
    new RichPoint(p)

  implicit def enrichGeometry(g: jts.Geometry): RichGeometry =
    new RichGeometry(g)
}
