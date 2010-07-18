package org.geoscript.geometry
import org.geoscript.projection.Projection

import com.vividsolutions.jts.{ geom => jts }
import org.geotools.geometry.jts.ReferencedEnvelope

object Box extends (jts.Envelope => Box) {
  import ModuleInternals.factory._

  class Wrapper(envelope: jts.Envelope) extends Box {
    def minX = envelope.getMinX()
    def maxX = envelope.getMaxX()
    def minY = envelope.getMinY()
    def maxY = envelope.getMaxY()
    def height = envelope.getHeight()
    def width = envelope.getWidth()

    lazy val underlying = 
      createPolygon(
        createLinearRing(
          Array(
            new jts.Coordinate(envelope.getMinX(), envelope.getMinY()),
            new jts.Coordinate(envelope.getMinX(), envelope.getMaxY()),
            new jts.Coordinate(envelope.getMaxX(), envelope.getMaxY()),
            new jts.Coordinate(envelope.getMaxX(), envelope.getMinY()),
            new jts.Coordinate(envelope.getMinX(), envelope.getMinY())
          )
        ),
        Array()
      )
    override val bounds = this
    override val prepared = true
    def prepare = this
    def in(proj: Projection) = new Projected(envelope, proj)
    override def transform(dest: Projection) = {
      val lr = Point(envelope.getMinX(), envelope.getMinY()) in projection
      val ul = Point(envelope.getMaxX(), envelope.getMaxY()) in projection
      val projected = new jts.Envelope(lr in dest, ul in dest)
      new Projected(projected, dest)
    }
  }

  class Projected(
    env: jts.Envelope,
    override val projection: Projection
  ) extends Wrapper(env) {
    override def in(dest: Projection) = transform(dest) 
  }

  def apply(minx: Double, miny: Double, maxx: Double, maxy: Double): Box =
    new Wrapper(new jts.Envelope(minx, maxx, miny, maxy))

  implicit def apply(env: jts.Envelope): Box = 
    env match {
      case projected: ReferencedEnvelope => 
        new Projected(projected, projected.getCoordinateReferenceSystem())
      case env => 
        new Wrapper(env)
    }

  implicit def unwrap(b: Box): ReferencedEnvelope =
    if (b.projection != null) 
      new ReferencedEnvelope(b.minX, b.maxX, b.minY, b.maxY, b.projection)
    else
      new ReferencedEnvelope(b.minX, b.maxX, b.minY, b.maxY, null)
}

trait Box extends Polygon {
  def minX: Double
  def maxX: Double
  def minY: Double
  def maxY: Double
  def height: Double
  def width: Double

  override def in(dest: Projection): Box
  override def transform(dest: Projection): Box = null
}
