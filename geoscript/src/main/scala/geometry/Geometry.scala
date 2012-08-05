package org.geoscript.geometry

import com.vividsolutions.jts.{ geom => jts }

class RichGeometry(geometry: Geometry) {
  /**
   * The area enclosed by this geometry, in the same units as used by its
   * coordinates.
   */
  def area: Double = geometry.getArea()

  /**
   * A symbolic alias for the union operation
   */
  def ||(that: Geometry): Geometry = geometry union that

  /**
   * A symbolic alias for the intersection operation
   */
  def &&(that: Geometry): Geometry = geometry intersection that

  /**
   * A jts.Envelope that fully encloses this Geometry.
   */
  def envelope: Envelope = geometry.getEnvelopeInternal()

  /**
   * A point that represents the "center of gravity" of this geometry's
   * enclosed area.  Note that this point is not necessarily on the geometry!
   */
  def centroid: Point = geometry.getCentroid()

  /**
   * All the coordinates that compose this Geometry as a sequence.
   */
  def coordinates: Seq[Coordinate] = geometry.getCoordinates().toSeq

  /**
   * prepare creates a PreparedGeometry instance, which requires more time to
   * construct but has faster implementations of several operations including
   * `contains`, `coveredBy`, `covers`, `crosses`, `disjoint`, 
   * `intersects`, `overlaps`, `touches`, and `within`
   *
   * Typically, this would be used when one geometry is being compared against
   * many others.
   * 
   * {{{
   * (needle: Geometry, haystack: Geometry) => {
   *   val p = needle.prepared
   *   haystack.filter(p.contains(_))
   * }
   * }}}
   */
  def prepare = preparingFactory.create(geometry)

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

  def && (that: Envelope): Envelope = e intersection that

  def || (that: Envelope): Envelope =
    if (e.isNull)         that
    else if (that.isNull) e
    else {
      val res = new jts.Envelope(e)
      res.expandToInclude(that)
      res
    }
}

class RichPoint(point: Point) extends RichGeometry(point) {
  def x = point.getX
  def y = point.getY
}
