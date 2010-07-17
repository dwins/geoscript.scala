package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the MultiPoint type, providing various methods for
 * directly instantiating MultiPoint objects.
 */
object MultiPoint {
  private val preparingFactory = new PreparedGeometryFactory()

  private class Wrapper(val underlying: jts.MultiPoint) extends MultiPoint {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.MultiPoint]
        new Wrapper(prep) {
          override def prepared = true
        }
      }

    def in(dest: Projection): MultiPoint = new Projected(underlying, dest)
  }

  private class Projected(
    val underlying: jts.MultiPoint, 
    override val projection: Projection
  ) extends MultiPoint {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.MultiPoint]
        new Projected(prep, projection) {
          override def prepared = true
        }
      }

    def in(dest: Projection): MultiPoint = 
      new Projected(projection.to(dest)(underlying), dest)
  }
  
  /**
   * Create a MultiPoint by wrapping a "raw" JTS MultiPoint
   */
  def apply(points: jts.MultiPoint): MultiPoint = new Wrapper(points) 

  /**
   * Create a MultiPoint from a list of input objects.  These objects can be
   * Points, JTS Points, JTS Coordinates, or tuples of numeric types.
   */
  def apply(coords: Seq[Any]): MultiPoint =
    new Wrapper(ModuleInternals.factory.createMultiPoint( 
      (coords map ModuleInternals.coerceCoord).toArray
    )) 
}

/**
 * A MultiPoint is a collection of 0 or more points that can be treated as a
 * single geometry.
 */
trait MultiPoint extends Geometry {
  override val underlying: jts.MultiPoint
  override def in(dest: Projection): MultiPoint
  override def transform(dest: Projection): MultiPoint = 
    MultiPoint(projection.to(dest)(underlying)) in dest
}
