package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the LineString type, providing various
 * methods for directly instantiating LineString objects.
 */
object LineString {
  /**
   * Create a LineString from JTS Coordinates.
   */
  def apply(coords: Point*): LineString =
    factory.createLineString(
      (coords map (_.getCoordinate))(collection.breakOut): Array[Coordinate]
    )
}

// /**
//  * A LineString contains 0 or more contiguous line segments, and is useful for
//  * representing geometries such as roads or rivers.
//  */
// trait LineString extends Geometry {
//   def vertices: Seq[Point]
//   override val underlying: jts.LineString
//   def isClosed: Boolean = underlying.isClosed
//   override def in(dest: Projection): LineString
//   override def transform(dest: Projection): LineString = 
//     LineString(projection.to(dest)(underlying)) in dest
// }
