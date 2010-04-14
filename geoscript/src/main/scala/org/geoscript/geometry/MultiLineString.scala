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
  private val preparingFactory = new PreparedGeometryFactory()
  private class Wrapper(val underlying: jts.MultiLineString) extends MultiLineString {
    override def members: Seq[LineString] = 
      0 until underlying.getNumGeometries map { n => 
        LineString(underlying.getGeometryN(n).asInstanceOf[jts.LineString])
      }
      
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

  private class Projected(
    val underlying: jts.MultiLineString, 
    override val projection: Projection
  ) extends MultiLineString {

    override def members: Seq[LineString] = 
      0 until underlying.getNumGeometries map { n => 
        LineString(
          underlying.getGeometryN(n).asInstanceOf[jts.LineString]
        ) in projection
      }

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

  def apply(lines: Iterable[LineString]): MultiLineString = 
    new Wrapper(ModuleInternals.factory.createMultiLineString(
      (lines map (_.underlying) toSeq) toArray
    ))

  def apply(lines: LineString*): MultiLineString = 
    new Wrapper(ModuleInternals.factory.createMultiLineString(
      lines map (_.underlying) toArray
    ))
  
  /**
   * Create a MultiLineString by wrapping a "raw" JTS MultiLineString.
   */
  def apply(lines: jts.MultiLineString): MultiLineString = new Wrapper(lines)
}

/**
 * A MultiLineString aggregates 0 or more line strings and allows them to be
 * treated as a single geometry.  For example, the length of a multilinestring
 * is the sum of the length of its constituent linestrings.
 */
trait MultiLineString extends Geometry {
  def members: Seq[LineString]
  override val underlying: jts.MultiLineString
  override def in(dest: Projection): MultiLineString
  override def transform(dest: Projection): MultiLineString = 
    MultiLineString(projection.to(dest)(underlying)) in dest
}
