package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

private object ModuleInternals {
  val factory = new jts.GeometryFactory() 

  def coerceCoord(input: Any) = {
    input match {
      case (x: Number, y: Number) => 
        new jts.Coordinate(x.doubleValue(), y.doubleValue())
      case (x: Number, y: Number, z: Number) =>
        new jts.Coordinate(x.doubleValue(), y.doubleValue(), z.doubleValue())
      case (p: jts.Point) => p.getCoordinate()
      case (coord: jts.Coordinate) => coord
      case other => throw new RuntimeException(
        "Coordinates can only be coerced from numeric tuples or points; " + 
        "found %s instead.".format(other)
      )
    }
  }
}

object EndCap {
  import com.vividsolutions.jts.operation.buffer.BufferOp._

  sealed abstract class Style { val intValue: Int }
  case object Butt extends Style { val intValue = CAP_BUTT }
  case object Round extends Style { val intValue = CAP_ROUND }
  case object Square extends Style { val intValue = CAP_SQUARE }
}

object Geometry {
  def apply(geom: jts.Geometry): Geometry = {
    geom match {
      case (point: jts.Point) => Point(point)
      case (poly: jts.Polygon) => Polygon(poly)
      case (linestring: jts.LineString) => LineString(linestring)
      case (multipoint: jts.MultiPoint) => MultiPoint(multipoint)
      case (multipoly: jts.MultiPolygon) => MultiPolygon(multipoly)
      case (multilinestring: jts.MultiLineString) => MultiLineString(multilinestring)
    }
  }

  def apply(geom: jts.Geometry, proj: Projection): Geometry =
    apply(geom) in proj
}

trait Geometry {
  val underlying: jts.Geometry

  def area: Double = underlying.getArea()

  def bounds: jts.Envelope = 
    underlying.getEnvelope().asInstanceOf[jts.Envelope] // in projection

  def centroid: Point = Point(underlying.getCentroid()) in projection

  def coordinates: Seq[Point] = 
    underlying.getCoordinates() map (c => Point(c) in projection)

  def length: Double = underlying.getLength()

  def json: String = "" // TODO: Real JSON encoding
  def wkt: String = underlying.toString()
  def projection: Projection = null // TODO: Real CRS tracking
  
  def buffer(dist: Double): Geometry = buffer(dist, 8, EndCap.Round)

  def buffer(dist: Double, segs: Int): Geometry = 
    buffer(dist, segs, EndCap.Round) 

  def buffer(dist: Double, segs: Int, mode: EndCap.Style): Geometry = 
    Geometry(underlying.buffer(dist, segs, mode.intValue), projection)

  def in(proj: Projection): Geometry

  def intersection(that: Geometry): Geometry = 
    Geometry(underlying intersection that.underlying)

  def isValid: Boolean = underlying.isValid

  def transform(dest: Projection): Geometry 
} 

