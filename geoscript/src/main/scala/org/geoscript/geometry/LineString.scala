package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object LineString {
  class Wrapper(val underlying: jts.LineString) extends LineString {
    def in(dest: Projection) = new Projected(underlying, dest)
  }

  class Projected(
    val underlying: jts.LineString,
    override val projection: Projection
  ) extends LineString {
    
    def in(dest: Projection) = 
      new Projected(projection.to(dest)(underlying), dest)
  }

  def apply(line: jts.LineString): LineString = new Wrapper(line)

  def apply(coords: jts.Coordinate*): LineString =
    new Wrapper(
      ModuleInternals.factory.createLineString(coords toArray)
    )
}

trait LineString extends Geometry {
  override val underlying: jts.LineString
  override def in(dest: Projection): LineString
  override def transform(dest: Projection): LineString = 
    LineString(projection.to(dest)(underlying)) in dest
}
