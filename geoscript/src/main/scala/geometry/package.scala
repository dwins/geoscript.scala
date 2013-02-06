package org.geoscript
package object geometry {
  import com.vividsolutions.jts.{geom => jts}

  type Geometry = jts.Geometry
  type GeometryCollection = jts.GeometryCollection
  type Polygon = jts.Polygon
  type MultiPolygon = jts.MultiPolygon
  type LineString = jts.LineString
  type MultiLineString = jts.MultiLineString
  type Point = jts.Point
  type MultiPoint = jts.MultiPoint
  
  type Coordinate = jts.Coordinate
  type Envelope = jts.Envelope

  val factory = new jts.GeometryFactory

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

    sealed abstract class Style(val intValue: Int)
    /** @see EndCap */
    case object Butt extends Style(CAP_FLAT)
    /** @see EndCap */
    case object Round extends Style(CAP_ROUND)
    /** @see EndCap */
    case object Square extends Style(CAP_SQUARE)
  }


  implicit class RichEnvelope(val envelope: jts.Envelope) extends AnyVal {
    def minX: Double = envelope.getMinX
    def maxX: Double = envelope.getMaxX
    def minY: Double = envelope.getMinY
    def maxY: Double = envelope.getMaxY

    def height: Double = envelope.getHeight
    def width: Double = envelope.getWidth

    def grid(branching: Int = 4): Iterator[jts.Envelope] = {
      val cellHeight = height / branching
      val cellWidth =  width / branching
      Iterator.tabulate(branching) { i =>
        Iterator.tabulate(branching) { j =>
          val minX = this.minX + i * cellWidth
          val minY = this.minY + j * cellHeight
          val maxX = minX + cellWidth
          val maxY = minY + cellHeight
          new Envelope(minX, minY, maxX, maxY)
      }}.flatten
    }

    def ** (that: Envelope): Envelope = {
      val clone = new Envelope(envelope)
      clone.expandToInclude(that)
      clone
    }
  }

  implicit class RichGeometry(val geometry: Geometry) extends AnyVal {
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
    def coordinates: Seq[Coordinate] = geometry.getCoordinates()

    /**
     * The length of the line segments that compose this geometry, in the same
     * units as used by its coordinates.
     */
    def length: Double = geometry.getLength()

    def mapVertices(op: Coordinate => Unit): Geometry = {
      val geom = geometry.clone().asInstanceOf[jts.Geometry]
      val filter = new FunctionAsCoordinateFilter(op)
      geom.apply(filter)
      geom
    }
  } 

  implicit class RichPoint(val p: Point) extends AnyVal {
    def x = p.getX
    def y = p.getY
  }
}

package geometry {
  class Builder(factory: com.vividsolutions.jts.geom.GeometryFactory) {
    def Coordinate(x: Double, y: Double): Coordinate = new Coordinate(x, y)
    def mkCoord(xy: (Double, Double)) = (Coordinate _).tupled(xy)

    def Envelope(minx: Double, maxx: Double, miny: Double, maxy: Double): Envelope =
      new com.vividsolutions.jts.geom.Envelope(minx, maxx, miny, maxy)

    def Point(x: Double, y: Double): Point =
      factory.createPoint(Coordinate(x,y))
    def LineString(coords: Seq[(Double, Double)]): LineString = 
      factory.createLineString(coords.map(mkCoord).toArray)
    def Polygon(ring: Seq[(Double, Double)], holes: Seq[Seq[(Double, Double)]] = Nil): Polygon =
      factory.createPolygon(
        factory.createLinearRing(ring.map(mkCoord).toArray),
        holes.map(h => factory.createLinearRing(h.map(mkCoord).toArray)).toArray)

    def MultiPoint(coords: Seq[(Double, Double)]): MultiPoint =
      factory.createMultiPoint(coords.map(mkCoord).toArray)
    def MultiLineString(lines: Seq[Seq[(Double, Double)]]): MultiLineString =
      factory.createMultiLineString(lines.map(LineString).toArray)
    def MultiPolygon(polys: Seq[(Seq[(Double, Double)], Seq[Seq[(Double, Double)]])]): MultiPolygon =
      factory.createMultiPolygon(polys.map((Polygon _).tupled).toArray)

    def multi(points: Seq[Point]): MultiPoint =
      factory.createMultiPoint(points.map(_.getCoordinate).toArray)
    def multi(lines: Seq[LineString]): MultiLineString =
      factory.createMultiLineString(lines.toArray)
    def multi(polys: Seq[Polygon]): MultiPolygon =
      factory.createMultiPolygon(polys.toArray)

    def collection(geoms: Seq[Geometry]): GeometryCollection =
      factory.createGeometryCollection(geoms.toArray)
  }

  object builder extends Builder(factory)

  private[geometry] class FunctionAsCoordinateFilter(f: Coordinate => Unit)
  extends com.vividsolutions.jts.geom.CoordinateFilter
  {
    def filter(coord: Coordinate) = f(coord)
  }
}
