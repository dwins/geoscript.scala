package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the Point type, providing various
 * methods for directly instantiating Point objects.
 */
object Point extends {

  def apply(coordinate: Coordinate) = factory.createPoint(coordinate)

  /**
   * Create a 3-dimensional point directly from coordinates.
   */
  def apply(x: Double, y: Double, z: Double): Point =
    factory.createPoint(new jts.Coordinate(x, y, z))

  /**
   * Create a 2-dimensional point directly from coordinates.
   */
  def apply(x: Double, y: Double): Point = 
    factory.createPoint(new jts.Coordinate(x, y))
}

class RichPoint(point: Point) extends RichGeometry(point) {
  def x = point.getX
  def y = point.getY
}
