package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object Polygon {
  private val preparingFactory = new PreparedGeometryFactory()
  import ModuleInternals.factory._

  class Wrapper(val underlying: jts.Polygon) extends Polygon {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.Polygon]
        new Wrapper(prep) {
          override def prepared = true
        }
      }

    override def in(dest: Projection): Polygon = new Projected(underlying, dest)
  }

  class Projected(
    val underlying: jts.Polygon,
    override val projection: Projection
  ) extends Polygon 
  {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.Polygon]
        new Projected(prep, projection) {
          override def prepared = true
        }
      }

    override def in(dest: Projection): Polygon = 
      new Projected(projection.to(dest)(underlying), dest)
  }

  def apply(wrapped: jts.Polygon): Polygon = new Wrapper(wrapped)

  def apply(shell: LineString, holes: Seq[LineString]): Polygon =
    new Wrapper(
      createPolygon(
        createLinearRing(shell.underlying.getCoordinateSequence()),
        holes map { 
          hole => createLinearRing(hole.underlying.getCoordinateSequence())
        } toArray
      )  
    )
}

trait Polygon extends Geometry {
  override val underlying: jts.Polygon

  override def in(dest: Projection): Polygon

  override def transform(dest: Projection): Polygon = 
    Polygon(projection.to(dest)(underlying)) in dest
}
