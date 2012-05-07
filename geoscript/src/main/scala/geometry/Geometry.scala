package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * An enumeration of the valid end-cap styles when buffering a (line) Geometry.
 * Valid styles include: 
 * <ul>
 *   <li>Round - A semicircle </li>
 *   <li>Butt - A straight line perpendicular to the end segment</li>
 *   <li>Square - A half-square</li>
 * </ul>
 * 
 * @see org.geoscript.geometry.Geometry.buffer
 */
object EndCap {
  // import com.vividsolutions.jts.operation.buffer.BufferOp._
  import com.vividsolutions.jts.operation.buffer.BufferParameters._

  sealed abstract class Style { val intValue: Int }
  /** @see EndCap */
  case object Butt extends Style { val intValue = CAP_FLAT }
  /** @see EndCap */
  case object Round extends Style { val intValue = CAP_ROUND }
  /** @see EndCap */
  case object Square extends Style { val intValue = CAP_SQUARE }
}

class RichGeometry(geometry: Geometry) {
  /**
   * The area enclosed by this geometry, in the same units as used by its
   * coordinates.
   */
  def area: Double = geometry.getArea()

  /**
   * A jts.Envelope that fully encloses this Geometry.
   */
  def envelope: Envelope = geometry.getEnvelopeInternal() // in projection

  /**
   * A point that represents the "center of gravity" of this geometry's
   * enclosed area.  Note that this point is not necessarily on the geometry!
   */
  def centroid: Point = geometry.getCentroid() // in projection

  /**
   * All the coordinates that compose this Geometry as a sequence.
   */
  def coordinates: Seq[Point] = 
    geometry.getCoordinates() map (c => Point(c)) //  in projection)

  /**
   * The length of the line segments that compose this geometry, in the same
   * units as used by its coordinates.
   */
  def length: Double = geometry.getLength()

  def mapVertices(op: Point => Point): Geometry = {
    val geom = geometry.clone().asInstanceOf[jts.Geometry]
    object filter extends jts.CoordinateFilter {
      def filter(coord: jts.Coordinate) = op(Point(coord)).getCoordinate
    }
    geom.apply(filter)
    geom
  }

  override def toString = geometry.toString
} 

class RichEnvelope(e: Envelope) {
  def maxY = e.getMaxY
  def maxX = e.getMaxX
  def minY = e.getMinY
  def minX = e.getMinX
}
