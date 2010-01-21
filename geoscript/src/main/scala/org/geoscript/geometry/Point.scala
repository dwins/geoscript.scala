package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

object Point {
  import ModuleInternals.factory._

  def apply(x: Double, y: Double, z: Double): jts.Point =
    createPoint(new jts.Coordinate(x, y, z))

  def apply(x: Double, y: Double): jts.Point = 
    createPoint(new jts.Coordinate(x, y))
}

class RichPoint(p: jts.Point) extends RichGeometry(p) {
  override def transform(dest: CoordinateReferenceSystem): jts.Point = 
    super.transform(dest).asInstanceOf[jts.Point]
}
