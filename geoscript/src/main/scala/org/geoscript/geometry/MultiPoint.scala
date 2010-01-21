package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

import org.geoscript.projection.Projection

object MultiPoint {
  import ModuleInternals.factory._

  def apply(coords: Seq[(Double,Double)]): jts.MultiPoint =
    createMultiPoint( 
      coords.map ({ elem => new jts.Coordinate(elem._1,elem._2) }).toArray
    ) 
}

trait MultiPoint extends Geometry {
  override val underlying: jts.MultiPoint
  override def in(dest: Projection): MultiPoint
}
