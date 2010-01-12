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
  def x: Double = p.getCoordinate.x
  def y: Double = p.getCoordinate.y
  def z: Double = p.getCoordinate.z

  override def clone(): jts.Point = p.clone().asInstanceOf[jts.Point]
  override def transform(dest: CoordinateReferenceSystem): jts.Point = 
    super.transform(dest).asInstanceOf[jts.Point]
}
