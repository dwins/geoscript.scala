package org.geoscript.geometry

import org.geoscript.projection.Projection
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import com.vividsolutions.jts.{geom => jts}

/**
 * A companion object for the GeometryCollection type, providing various
 * methods for directly instantiating GeometryCollection objects.
 */
object GeometryCollection {
  def apply(geoms: Geometry*): GeometryCollection = 
    factory.createGeometryCollection(geoms.toArray)
}

// /**
//  * A GeometryCollection aggregates 0 or more Geometry objects together and
//  * allows spatial calculations to be performed against the collection as if it
//  * were a single geometry.  For example, the area of the collection is simply
//  * the sum of the areas of its constituent geometry objects.
//  */
// trait GeometryCollection extends Geometry {
//   def members: Seq[Geometry]
//   override val underlying: jts.GeometryCollection
//   override def in(proj: Projection): GeometryCollection
//   override def transform(dest: Projection): GeometryCollection = 
//     GeometryCollection(projection.to(dest)(underlying)) in dest
// }
