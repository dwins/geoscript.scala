package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object MultiPoint {
   class Wrapper(val underlying: jts.MultiPoint) extends MultiPoint {
    def in(dest: Projection): MultiPoint = new Projected(underlying, dest)
  }

  class Projected(
    val underlying: jts.MultiPoint, 
    override val projection: Projection
  ) extends MultiPoint {
    def in(dest: Projection): MultiPoint = 
      new Projected(projection.to(dest)(underlying), dest)
  }
  
  def apply(mp : jts.MultiPoint): MultiPoint = new Wrapper(mp) 

  def apply(coords: Seq[(Double,Double)]): MultiPoint =
    new Wrapper(ModuleInternals.factory.createMultiPoint( 
      coords.map ({ elem => new jts.Coordinate(elem._1,elem._2) }).toArray
    )) 
}

trait MultiPoint extends Geometry {
  override val underlying: jts.MultiPoint
  override def in(dest: Projection): MultiPoint

  override def transform(dest: Projection): MultiPoint = 
    MultiPoint(projection.to(dest)(underlying)) in dest



}
