package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object Point {
  private val preparingFactory = new PreparedGeometryFactory()

  class Wrapper(val underlying: jts.Point) extends Point {
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

  class Projected (
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
      new Projected(projection.to(dest)(underlying), dest)
  }

  def apply(x: Double, y: Double, z: Double): Point =
    new Wrapper(
      ModuleInternals.factory.createPoint(new jts.Coordinate(x, y, z))
    )

  def apply(x: Double, y: Double): Point = 
    new Wrapper(
      ModuleInternals.factory.createPoint(new jts.Coordinate(x, y))
    )

  def apply(p: jts.Point): Point = new Wrapper(p)

  def apply(p: jts.Point, proj: Projection): Point = new Projected(p, proj)

  def apply(c: jts.Coordinate): Point = 
    new Wrapper(ModuleInternals.factory.createPoint(c))
}

trait Point extends Geometry {
  override val underlying: jts.Point

  def x = underlying.getCoordinate().x
  def y = underlying.getCoordinate().y
  def z = underlying.getCoordinate().z

  override def in(proj: Projection): Point

  override def transform(dest: Projection): Point = 
    Point(projection.to(dest)(underlying)) in dest
}
