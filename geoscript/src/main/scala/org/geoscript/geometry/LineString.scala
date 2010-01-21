package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

object LineString {
  def apply(coords: jts.Coordinate*): jts.LineString =
    ModuleInternals.factory.createLineString(coords toArray)
}

class RichLineString(p: jts.LineString) extends RichGeometry(p) {
  override def clone(): jts.LineString = p.clone().asInstanceOf[jts.LineString]
  override def transform(dest: CoordinateReferenceSystem): jts.LineString = 
    super.transform(dest).asInstanceOf[jts.LineString]
}
