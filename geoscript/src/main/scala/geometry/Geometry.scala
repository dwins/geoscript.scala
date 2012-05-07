package org.geoscript.geometry

import com.vividsolutions.jts.{ geom => jts }

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
  def coordinates: Seq[Coordinate] = geometry.getCoordinates().toSeq

  /**
   * The length of the line segments that compose this geometry, in the same
   * units as used by its coordinates.
   */
  def length: Double = geometry.getLength()

  override def toString = geometry.toString
} 

class RichEnvelope(e: Envelope) {
  def maxY = e.getMaxY
  def maxX = e.getMaxX
  def minY = e.getMinY
  def minX = e.getMinX
}

class RichPoint(point: Point) extends RichGeometry(point) {
  def x = point.getX
  def y = point.getY
}
