package org.geoscript.projection

import com.vividsolutions.jts.{geom => jts}

import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

/**
 * A Projection is a particular system of representing real-world locations
 * with numeric coordinates.  For example, Projection("EPSG:4326") is the
 * system of representing positions with (longitude, latitude) pairs.
 */
class Projection(val crs: CoordinateReferenceSystem) {
  /**
   * Create a conversion function from this projection to another one, which
   * can be applied to JTS Geometries. Example usage:
   * 
   * <pre> 
   * val jtsGeom = Point(1, 2).underlying
   * val convert: jts.Point => jts.Point = Projection("EPSG:4326") to Projection("EPSG:3785")
   * val reprojected = convert(jtsGeom)
   * </pre>
   */
  def to[Geom<:jts.Geometry](dest: Projection)(geom: Geom) = {
    val tx = CRS.findMathTransform(crs, dest.crs)
    JTS.transform(geom, tx).asInstanceOf[Geom] 
  }

  /**
   * Get the official spatial reference identifier for this projection, if any
   */
  def id: String = CRS.toSRS(crs)

  /**
   * Get the Well Known Text specification of this projection.
   * 
   * @see http://en.wikipedia.org/wiki/Well-known_text
   */
  def wkt: String = crs.toString()

  override def toString: String = 
    id match {
      case null => "<Unidentified projection>"
      case id => id
    }
}

/**
 * The Projection object provides several methods for getting Projection
 * instances.
 */
object Projection {
  val forceXY = System.getProperty("org.geotools.referencing.forceXY")

    if (forceXY == null || forceXY.toBoolean == false)
    System.setProperty("org.geotools.referencing.forceXY", "true")

  if (Hints.getSystemDefault(Hints.FORCE_LONGITUDE_FIRST_AXIS_ORDER)
      .asInstanceOf[Boolean])
    Hints.putSystemDefault(Hints.FORCE_AXIS_ORDER_HONORING, "http")

  /**
   * Get a Projection instance by either looking up an identifier in the
   * projection database, or decoding a Well-Known Text definition.
   *
   * @see http://en.wikipedia.org/wiki/Well-known_text
   */
  def apply(s: String): Projection = {
    new Projection(
      try {
        CRS.decode(s)
      } catch {
        case _ => CRS.parseWKT(s)
      }
    )
  }

  /**
   * Create a Projection by wrapping a GeoTools CoordinateReferenceSystem
   * object.
   */
  def apply(crs: CoordinateReferenceSystem) = new Projection(crs)
}

/**
 * The Implicits object for projections provides several convenience methods
 * for automatically wrapping and unwrapping projections.  For example:
 * <pre>
 * val p = Point(-8, 17) in "EPSG:4326"
 * val p_projected = p in "EPSG:3785"
 * </pre>
 */
trait Implicits {
  implicit def codeToCRS(code: String) = Projection(code)
  implicit def wrapCRS(crs: CoordinateReferenceSystem) = Projection(crs)
  implicit def unwrapCRS(proj: Projection) = proj.crs
}
