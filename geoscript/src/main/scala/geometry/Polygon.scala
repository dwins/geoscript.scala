package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the Polygon type, providing various methods for
 * directly instantiating Polygon objects.
 */
object Polygon extends (jts.Polygon => Polygon) {
  private val preparingFactory = new PreparedGeometryFactory()
  import ModuleInternals.factory._

  private class Wrapper(val underlying: jts.Polygon) extends Polygon {
    def shell: LineString = LineString(underlying.getExteriorRing())
    def holes: Seq[LineString] = 
      0 until underlying.getNumInteriorRing map {
        n => LineString(underlying.getInteriorRingN(n))
      }

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

  private class Projected(
    val underlying: jts.Polygon,
    override val projection: Projection
  ) extends Polygon {
    def shell: LineString =
      LineString(underlying.getExteriorRing()) in projection

    def holes: Seq[LineString] = 
      0 until underlying.getNumInteriorRing map {
        n => LineString(underlying.getInteriorRingN(n)) in projection
      }

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

  /**
   * Create a Polygon by wrapping a "raw" JTS Polygon.
   */
  implicit def apply(wrapped: jts.Polygon): Polygon = new Wrapper(wrapped)

  implicit def unwrap(wrapped: Polygon): jts.Polygon = wrapped.underlying

  /**
   * Create a Polygon from an outer shell and a list of zero or more holes.
   */
  def apply(shell: LineString, holes: Seq[LineString] = Nil): Polygon =
    new Wrapper(
      createPolygon(
        createLinearRing(shell.underlying.getCoordinateSequence()),
        holes map { 
          hole => createLinearRing(hole.underlying.getCoordinateSequence())
        } toArray
      )  
    )
}

/**
 * A polygon represents a contiguous area, possibly with holes.
 */
trait Polygon extends Geometry {
  def shell: LineString
  def holes: Seq[LineString]
  def rings: Seq[LineString] = Seq(shell) ++ holes
  override val underlying: jts.Polygon
  override def in(dest: Projection): Polygon
  override def transform(dest: Projection): Polygon = 
    Polygon(projection.to(dest)(underlying)) in dest
}
