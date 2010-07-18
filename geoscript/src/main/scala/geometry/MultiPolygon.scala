
package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the MultiPolygon type, providing various
 * methods for directly instantiating MultiPolygon objects.
 */
object MultiPolygon {
  private val preparingFactory = new PreparedGeometryFactory()

  private class Wrapper(val underlying: jts.MultiPolygon) extends MultiPolygon {
    override def prepare() = 
    if (prepared) {
      this
    } else {
      val prep =
        preparingFactory.create(underlying).asInstanceOf[jts.MultiPolygon]
      new Wrapper(prep) {
        override def prepared = true
      }
    }

    def in(dest: Projection): MultiPolygon = new Projected(underlying, dest)
  }

  private class Projected(
    val underlying: jts.MultiPolygon, 
    override val projection: Projection
  ) extends MultiPolygon {

    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.MultiPolygon]
        new Projected(prep, projection) {
          override def prepared = true
        }
      }

    def in(dest: Projection): MultiPolygon= 
      new Projected(projection.to(dest)(underlying), dest)
  }
  
  /**
   * Create a MultiPolygon by wrapping a "raw" JTS MultiPolygon.
   */
  implicit def apply(polygons: jts.MultiPolygon): MultiPolygon =
    new Wrapper(polygons) 

  implicit def apply(polygons: MultiPolygon): jts.MultiPolygon =
    polygons.underlying

  /**
   * Create a MultiPolygon from a sequence of Polygons
   */
  def apply(polygons: Seq[jts.Polygon]): MultiPolygon =
    new Wrapper( 
      ModuleInternals.factory.createMultiPolygon(polygons.toArray) 
    ) 
}

/**
 * A MultiPolygon is a collection of 0 or more polygons that can be treated as
 * a single geometry.
 */
trait MultiPolygon extends Geometry {
  override val underlying: jts.MultiPolygon
  override def in(dest: Projection): MultiPolygon
  override def transform(dest: Projection): MultiPolygon = 
    MultiPolygon(projection.to(dest)(underlying)) in dest
}
