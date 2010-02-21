package org.geoscript.geometry

import org.geoscript.projection.Projection
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import com.vividsolutions.jts.{geom => jts}

object GeometryCollection {
  private val preparingFactory = new PreparedGeometryFactory()
  class Wrapped(val underlying: jts.GeometryCollection) extends GeometryCollection {
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

  class Projected (
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

  def apply(raw: jts.GeometryCollection) = new Wrapped(raw)
}

trait GeometryCollection extends Geometry {
  override val underlying: jts.GeometryCollection
  override def in(proj: Projection): GeometryCollection
  override def transform(dest: Projection): GeometryCollection = 
    GeometryCollection(projection.to(dest)(underlying)) in dest
}
