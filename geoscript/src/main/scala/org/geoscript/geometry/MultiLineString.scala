package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

object MultiLineString {
  private val preparingFactory = new PreparedGeometryFactory()
  class Wrapper(val underlying: jts.MultiLineString) extends MultiLineString {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep = preparingFactory
            .create(underlying)
            .asInstanceOf[jts.MultiLineString]

        new Wrapper(prep) {
          override def prepared = true
        }
      }

    def in(dest: Projection): MultiLineString = new Projected(underlying, dest)
  }

  class Projected(
    val underlying: jts.MultiLineString, 
    override val projection: Projection
  ) extends MultiLineString {
    override def prepare() = 
      if (prepared) {
        this
      } else {
        val prep = preparingFactory
          .create(underlying)
          .asInstanceOf[jts.MultiLineString]
        new Projected(prep, projection) {
          override def prepared = true
        }
      }

    def in(dest: Projection): MultiLineString = 
      new Projected(projection.to(dest)(underlying), dest)
  }
  
  def apply(mp : jts.MultiLineString): MultiLineString = new Wrapper(mp)

  def apply(ls : Seq[jts.LineString]): MultiLineString = 
    new Wrapper( 
    ModuleInternals.factory.createMultiLineString(ls.toArray) 
  )

}

trait MultiLineString extends Geometry {
  override val underlying: jts.MultiLineString
  override def in(dest: Projection): MultiLineString

  override def transform(dest: Projection): MultiLineString = 
    MultiLineString(projection.to(dest)(underlying)) in dest

}
