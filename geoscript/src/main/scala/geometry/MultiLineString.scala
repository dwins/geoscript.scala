package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection


/**
 * A companion object for the MultiLineString type, providing various
 * methods for directly instantiating MultiLineString objects.
 */
object MultiLineString {
  /**
   * Create a MultiLineString from a list of JTS LineStrings
   */
  def apply(lines: Iterable[LineString]): MultiLineString = 
    factory.createMultiLineString(lines.toArray) 

  def apply(lines: LineString*): MultiLineString = 
    factory.createMultiLineString(lines.toArray)
}

// /**
//  * A MultiLineString aggregates 0 or more line strings and allows them to be
//  * treated as a single geometry.  For example, the length of a multilinestring
//  * is the sum of the length of its constituent linestrings.
//  */
// trait MultiLineString extends Geometry {
//   def members: Seq[LineString]
//   override val underlying: jts.MultiLineString
//   override def in(dest: Projection): MultiLineString
//   override def transform(dest: Projection): MultiLineString = 
//     MultiLineString(projection.to(dest)(underlying)) in dest
// }
