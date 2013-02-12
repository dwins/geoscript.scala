
package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the MultiPolygon type, providing various
 * methods for directly instantiating MultiPolygon objects.
 */
object MultiPolygon {
  def apply(polygons: Iterable[Polygon]): MultiPolygon = 
    factory.createMultiPolygon(polygons.toArray)

  def apply(polygons: Polygon*): MultiPolygon = 
    factory.createMultiPolygon(polygons.toArray)
}

// /**
//  * A MultiPolygon is a collection of 0 or more polygons that can be treated as
//  * a single geometry.
//  */
// trait MultiPolygon extends Geometry {
//   def members: Seq[Polygon]
//   override val underlying: jts.MultiPolygon
//   override def in(dest: Projection): MultiPolygon
//   override def transform(dest: Projection): MultiPolygon = 
//     MultiPolygon(projection.to(dest)(underlying)) in dest
// }
