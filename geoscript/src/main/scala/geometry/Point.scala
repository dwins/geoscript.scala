package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the Point type, providing various
 * methods for directly instantiating Point objects.
 */
object Point extends (jts.Point => Point) {
  private val preparingFactory = new PreparedGeometryFactory()

  private class Wrapper(val underlying: jts.Point) extends Point {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.Point]
        new Wrapper(prep) {
          override def prepared = true
        }
      }

    def in(dest: Projection): Point = new Projected(underlying, dest)
  }

  private class Projected (
    val underlying: jts.Point, 
    override val projection: Projection
  ) extends Point {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.Point]
        new Projected(prep, projection) {
          override def prepared = true
        }
      }

    def in(dest: Projection): Point = 
      new Projected((projection to dest)(underlying), dest)
  }

  /**
   * Create a 3-dimensional point directly from coordinates.
   */
  def apply(x: Double, y: Double, z: Double): Point =
    new Wrapper(
      ModuleInternals.factory.createPoint(new jts.Coordinate(x, y, z))
    )

  /**
   * Create a 2-dimensional point directly from coordinates.
   */
  def apply(x: Double, y: Double): Point = 
    new Wrapper(
      ModuleInternals.factory.createPoint(new jts.Coordinate(x, y))
    )

  def apply(tuple: (Double, Double)): Point = apply(tuple._1, tuple._2)

  def apply(tuple: (Double, Double, Double)): Point = 
    apply(tuple._1, tuple._2, tuple._3)

  /**
   * Create a Point by wrapping a "raw" JTS Point.
   */
  implicit def apply(p: jts.Point): Point = new Wrapper(p)

  implicit def unwrap(p: Point): jts.Point = p.underlying
  implicit def toCoord(p: Point): jts.Coordinate = p.underlying.getCoordinate()

  /**
   * Create a Point by wrapping a "raw" JTS Point with a projection.
   */
  def apply(p: jts.Point, proj: Projection): Point = new Projected(p, proj)

  /**
   * Create a Point by wrapping a JTS Coordinate
   */
  def apply(c: jts.Coordinate): Point = 
    new Wrapper(ModuleInternals.factory.createPoint(c))
}

/**
 * A Point represents a distinct location in space.
 */
trait Point extends Geometry {
  override val underlying: jts.Point
  /**
   * The Point's position along the "horizontal" axis (note this is
   * projection-dependent.)
   */
  def x = underlying.getCoordinate().x

  /**
   * The Point's position along the "vertical" axis (note this is
   * projection-dependent.)
   */
  def y = underlying.getCoordinate().y

  /**
   * The Point's height (note this is projection-dependent.)
   */
  def z = underlying.getCoordinate().z
  override def in(proj: Projection): Point
  override def transform(dest: Projection): Point = 
    Point(projection.to(dest)(underlying)) in dest
}
