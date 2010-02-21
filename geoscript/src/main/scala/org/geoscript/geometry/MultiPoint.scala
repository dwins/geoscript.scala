package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object MultiPoint {
  private val preparingFactory = new PreparedGeometryFactory()

  class Wrapper(val underlying: jts.MultiPoint) extends MultiPoint {
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

  class Projected(
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
  
  def apply(mp : jts.MultiPoint): MultiPoint = new Wrapper(mp) 

  def apply(coords: Seq[Any]): MultiPoint =
    new Wrapper(ModuleInternals.factory.createMultiPoint( 
      (coords map ModuleInternals.coerceCoord).toArray
    )) 
}

trait MultiPoint extends Geometry {
  override val underlying: jts.MultiPoint
  override def in(dest: Projection): MultiPoint

  override def transform(dest: Projection): MultiPoint = 
    MultiPoint(projection.to(dest)(underlying)) in dest



}
