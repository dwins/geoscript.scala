package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object MultiLineString {
   class Wrapper(val underlying: jts.MultiLineString) extends MultiLineString {
    def in(dest: Projection): MultiLineString = new Projected(underlying, dest)
  }

  class Projected(
    val underlying: jts.MultiLineString, 
    override val projection: Projection
  ) extends MultiLineString {
    def in(dest: Projection): MultiLineString = 
      new Projected(projection.to(dest)(underlying), dest)
  }
  
  def apply(mp : jts.MultiLineString): MultiLineString = new Wrapper(mp) 

}

trait MultiLineString extends Geometry {
  override val underlying: jts.MultiLineString
  override def in(dest: Projection): MultiLineString

  override def transform(dest: Projection): MultiLineString = 
    MultiLineString(projection.to(dest)(underlying)) in dest



}
