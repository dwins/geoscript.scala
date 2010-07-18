package org.geoscript.geometry

import org.geoscript.projection.Projection
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import com.vividsolutions.jts.{geom => jts}

/**
 * A companion object for the GeometryCollection type, providing various
 * methods for directly instantiating GeometryCollection objects.
 */
object GeometryCollection {
  private val preparingFactory = new PreparedGeometryFactory()
  private class Wrapped(val underlying: jts.GeometryCollection) extends GeometryCollection {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.GeometryCollection]
        new Wrapped(prep) {
          override def prepared = true
        }
      }

    override def in(proj: Projection) = new Projected(underlying, proj)
  }

  private class Projected (
    val underlying: jts.GeometryCollection,
    override val projection: Projection
  ) extends GeometryCollection {

    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep =
          preparingFactory.create(underlying).asInstanceOf[jts.GeometryCollection]
        new Projected(prep, projection) {
          override def prepared = true
        }
      }

    override def in(dest: Projection) =
      new Projected(projection.to(dest)(underlying), dest)
  }

  /**
   * Create a GeometryCollection by wrapping a "raw" JTS GeometryCollection.
   */
  implicit def apply(raw: jts.GeometryCollection): GeometryCollection =
    new Wrapped(raw)

  implicit def unwrap(wrapped: GeometryCollection): jts.GeometryCollection =
    wrapped.underlying
}

/**
 * A GeometryCollection aggregates 0 or more Geometry objects together and
 * allows spatial calculations to be performed against the collection as if it
 * were a single geometry.  For example, the area of the collection is simply
 * the sum of the areas of its constituent geometry objects.
 */
trait GeometryCollection extends Geometry {
  override val underlying: jts.GeometryCollection
  override def in(proj: Projection): GeometryCollection
  override def transform(dest: Projection): GeometryCollection = 
    GeometryCollection(projection.to(dest)(underlying)) in dest
}
