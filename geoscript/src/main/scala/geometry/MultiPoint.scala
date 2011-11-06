package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the MultiPoint type, providing various methods for
 * directly instantiating MultiPoint objects.
 */
object MultiPoint {
  /**
   * Create a MultiPoint from a list of input objects.  These objects can be
   * Points, JTS Points, JTS Coordinates, or tuples of numeric types.
   */
  def apply(coords: Point*): MultiPoint =
    factory.createMultiPoint(coords.toArray)
}
