package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

object MultiPoint {
  import ModuleInternals.factory._

  def apply(coords: Seq[(Double,Double)]): jts.MultiPoint =
    createMultiPoint( 
      coords.map ({ elem => new jts.Coordinate(elem._1,elem._2) }).toArray
    ) 
}

class RichMultiPoint(p: jts.Point) extends RichGeometry(p) {
  override def clone(): jts.Point = p.clone().asInstanceOf[jts.Point]
  override def transform(dest: CoordinateReferenceSystem): jts.Point = 
    super.transform(dest).asInstanceOf[jts.Point]
}
