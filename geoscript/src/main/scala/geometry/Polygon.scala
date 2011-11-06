package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * A companion object for the Polygon type, providing various methods for
 * directly instantiating Polygon objects.
 */
object Polygon {
  /**
   * Create a Polygon from an outer shell and a list of zero or more holes.
   */
  def apply(shell: LineString, holes: Seq[LineString] = Nil): Polygon =
    factory.createPolygon(
      factory.createLinearRing(shell.getCoordinateSequence()),
      holes.map(hole =>
        factory.createLinearRing(hole.getCoordinateSequence())
      )(collection.breakOut)
    )  
}

// /**
//  * A polygon represents a contiguous area, possibly with holes.
//  */
// trait Polygon extends Geometry {
//   def shell: LineString
//   def holes: Seq[LineString]
//   def rings: Seq[LineString] = Seq(shell) ++ holes
//   override val underlying: jts.Polygon
//   override def in(dest: Projection): Polygon
//   override def transform(dest: Projection): Polygon = 
//     Polygon(projection.to(dest)(underlying)) in dest
// }
