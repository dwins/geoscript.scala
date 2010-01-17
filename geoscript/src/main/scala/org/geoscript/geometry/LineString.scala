package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

object LineString {
  import ModuleInternals.factory._

  def apply(coordTuple: Any*): jts.LineString =  {
    createLineString(ModuleInternals.makeCoordSeq(coordTuple: _*))
  }

}

class RichLineString(p: jts.LineString) extends RichGeometry(p) {

  override def clone(): jts.LineString = p.clone().asInstanceOf[jts.LineString]
  override def transform(dest: CoordinateReferenceSystem): jts.LineString = 
    super.transform(dest).asInstanceOf[jts.LineString]
}
