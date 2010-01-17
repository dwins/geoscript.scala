package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem

object Polygon {
  import ModuleInternals.factory._

  def apply(shell: jts.LinearRing, holes: Array[jts.LinearRing]): jts.Polygon =  { 
    createPolygon(shell,holes)  
  }
  def apply(shell: Seq[(Double,Double)], holes: Seq[Seq[(Double,Double)]]): jts.Polygon = { 
    createPolygon(
      createLinearRing(
        new jts.impl.CoordinateArraySequence(shell.map ({
            elem => new jts.Coordinate(elem._1,elem._2)
        }).toArray
      )),
      holes.map ({ 
        elem => createLinearRing(ModuleInternals.makeCoordSeq(elem.toArray: _*)) 
      }).toArray  
    )
  } 

}

class RichPolygon(p: jts.Polygon) extends RichGeometry(p) {

  override def clone(): jts.Polygon = p.clone().asInstanceOf[jts.Polygon]
  override def transform(dest: CoordinateReferenceSystem): jts.Polygon = 
    super.transform(dest).asInstanceOf[jts.Polygon]
}
