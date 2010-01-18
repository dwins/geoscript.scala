package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

private object ModuleInternals {
  val factory = new jts.GeometryFactory() 
  def makeCoordSeq(input: Any* ) = { 
    val coordSeq = input.map  ({ 
      case (x: Double,y: Double) => new jts.Coordinate(x,y) 
      case (x: Double, y: Double, z: Double) => new jts.Coordinate(x,y,z)  
      case p: jts.Point => p.getCoordinate
      case _  => throw new RuntimeException("LineString requires CoordinateSeq")  
    }).toArray
    new jts.impl.CoordinateArraySequence(coordSeq) 
   } 

}

object EndCap {
  import com.vividsolutions.jts.operation.buffer.BufferOp._

  sealed abstract class Style { val intValue: Int }
  case object Butt extends Style { val intValue = CAP_BUTT }
  case object Round extends Style { val intValue = CAP_ROUND }
  case object Square extends Style { val intValue = CAP_SQUARE }
}

class RichGeometry(geom: jts.Geometry) {
  def area: Double = geom.getArea()
  def bounds: jts.Envelope = geom.getEnvelope().asInstanceOf[jts.Envelope]
  def centroid: jts.Point = geom.getCentroid()
  def coordinates: Seq[jts.Coordinate] = geom.getCoordinates()
  def length: Double = geom.getLength()

  def json: String = "" // TODO: Real JSON encoding
  def wkt: String = geom.toString()
  def projection: CoordinateReferenceSystem = null // TODO: Real CRS tracking
  
  def buffer(dist: Double): jts.Geometry =
    buffer(dist, 8, EndCap.Round)

  def buffer(dist: Double, segs: Int): jts.Geometry = 
    buffer(dist, segs, EndCap.Round) 

  def buffer(dist: Double, segs: Int, mode: EndCap.Style): jts.Geometry = 
    geom.buffer(dist, segs, mode.intValue)

  override def clone(): jts.Geometry = geom.clone().asInstanceOf[jts.Geometry]

  def transform(dest: CoordinateReferenceSystem): jts.Geometry = {
    // TODO: Provide an implicit in the projection package that gives this
    // signature, but actually looks up the MathTransform and performs the
    // transformation
    implicit def x(c: CoordinateReferenceSystem) = new {
      def to (d: CoordinateReferenceSystem)(g: jts.Geometry) = g
    }
    (projection to dest)(geom)
  }
} 

