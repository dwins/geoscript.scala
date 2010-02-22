
package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object MultiPolygon {
  private val preparingFactory = new PreparedGeometryFactory()

  class Wrapper(val underlying: jts.MultiPolygon) extends MultiPolygon {
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

  class Projected(
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
  
  def apply(mp : jts.MultiPolygon): MultiPolygon = new Wrapper(mp) 

  def apply(polygons : Seq[jts.Polygon]): MultiPolygon =
  new Wrapper( 
    ModuleInternals.factory.createMultiPolygon(polygons.toArray) 
    ) 

}

trait MultiPolygon extends Geometry {
  override val underlying: jts.MultiPolygon
  override def in(dest: Projection): MultiPolygon

  override def transform(dest: Projection): MultiPolygon = 
    MultiPolygon(projection.to(dest)(underlying)) in dest

}