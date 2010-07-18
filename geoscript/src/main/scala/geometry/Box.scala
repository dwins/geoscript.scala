package org.geoscript.geometry
import org.geoscript.projection.Projection

import com.vividsolutions.jts.{ geom => jts }

object Box extends (jts.Envelope => Box) {
  import ModuleInternals.factory._

  class Wrapper(override val bounds: jts.Envelope) extends Box {
    lazy val underlying = 
      createPolygon(
        createLinearRing(
          Array(
            new jts.Coordinate(bounds.getMinX(), bounds.getMinY()),
            new jts.Coordinate(bounds.getMinX(), bounds.getMaxY()),
            new jts.Coordinate(bounds.getMaxX(), bounds.getMaxY()),
            new jts.Coordinate(bounds.getMaxX(), bounds.getMinY()),
            new jts.Coordinate(bounds.getMinX(), bounds.getMinY())
          )
        ),
        Array()
      )
    override val prepared = true
    def prepare = this
    def in(proj: Projection) = new Projected(bounds, proj)
    override def transform(dest: Projection) = {
      val lr = Point(bounds.getMinX(), bounds.getMinY()) in projection
      val ul = Point(bounds.getMaxX(), bounds.getMaxY()) in projection
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

  implicit def apply(env: jts.Envelope): Box = new Wrapper(env)
  implicit def unwrap(b: Box): jts.Envelope = b.bounds
}

trait Box extends Polygon {
  override def in(dest: Projection): Box
  override def transform(dest: Projection): Box = null
}
