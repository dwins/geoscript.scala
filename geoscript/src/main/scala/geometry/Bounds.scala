package org.geoscript.geometry
import org.geoscript.projection.Projection

import com.vividsolutions.jts.{ geom => jts }
import org.geotools.geometry.jts.ReferencedEnvelope

object Bounds extends (jts.Envelope => Bounds) {
  import ModuleInternals.factory._

  class Wrapper(envelope: jts.Envelope) extends Bounds {
    def minX = envelope.getMinX()
    def maxX = envelope.getMaxX()
    def minY = envelope.getMinY()
    def maxY = envelope.getMaxY()
    def height = envelope.getHeight()
    def width = envelope.getWidth()

    def shell: LineString = 
      LineString(
        Point(minX, minY),
        Point(minX, maxY),
        Point(maxX, maxY),
        Point(maxX, minY),
        Point(minX, minY)
      )
    def holes: Seq[LineString] = Nil

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

  def apply(minx: Double, miny: Double, maxx: Double, maxy: Double): Bounds =
    new Wrapper(new jts.Envelope(minx, maxx, miny, maxy))

  implicit def apply(env: jts.Envelope): Bounds = 
    env match {
      case projected: ReferencedEnvelope 
      if projected.getCoordinateReferenceSystem != null
      => 
        new Projected(projected, projected.getCoordinateReferenceSystem())
      case env => 
        new Wrapper(env)
    }

  implicit def unwrap(b: Bounds): ReferencedEnvelope =
    if (b.projection != null) 
      new ReferencedEnvelope(b.minX, b.maxX, b.minY, b.maxY, b.projection)
    else
      new ReferencedEnvelope(b.minX, b.maxX, b.minY, b.maxY, null)
}

trait Bounds extends Polygon {
  def minX: Double
  def maxX: Double
  def minY: Double
  def maxY: Double
  def height: Double
  def width: Double

  def grid(granularity: Int = 4): Iterable[Bounds] =
    (for {
      x <- (0 to granularity).sliding(2)
      y <- (0 to granularity).sliding(2)
    } yield {
      Bounds(
        minX + (x(0) * width / granularity),
        minY + (y(0) * height / granularity),
        minX + (x(1) * width / granularity),
        minY + (y(1) * height / granularity)
      ) in projection
    }) toIterable

  def expand(that: Bounds): Bounds = {
    import math.{ min, max }
    val result = 
      Bounds(
        min(this.minX, that.minX), min(this.minY, that.minY),
        max(this.maxX, that.maxX), max(this.maxY, that.maxY)
      )
    if (projection != null)
      result in projection
    else
      result
  }

  override def in(dest: Projection): Bounds
  override def transform(dest: Projection): Bounds = null
  override def toString =
    "Bounds((%f, %f), (%f, %f))".format(minX, minY, maxX, maxY)
}
