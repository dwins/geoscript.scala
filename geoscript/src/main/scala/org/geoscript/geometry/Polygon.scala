package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

object Polygon {
  def apply(shell: jts.LinearRing, holes: Seq[jts.LinearRing]): jts.Polygon =
    ModuleInternals.factory.createPolygon(shell, holes.toArray)  
}

class RichPolygon(p: jts.Polygon) extends RichGeometry(p) {
  override def clone(): jts.Polygon = p.clone().asInstanceOf[jts.Polygon]
  override def transform(dest: CoordinateReferenceSystem): jts.Polygon = 
    super.transform(dest).asInstanceOf[jts.Polygon]
}
