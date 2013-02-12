package org.geoscript

import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

package object projection {
  type Projection = org.opengis.referencing.crs.CoordinateReferenceSystem
  
  def lookupEPSG(code: String): Option[Projection] =
    try
      Some(org.geotools.referencing.CRS.decode(code))
    catch {
      case (_: org.opengis.referencing.FactoryException) => None
    }

  def parseWKT(text: String): Option[Projection] =
    try
      Some(org.geotools.referencing.CRS.parseWKT(text))
    catch {
      case (_: org.opengis.referencing.FactoryException) => None
    }

  /**
   * A Projection is a particular system of representing real-world locations
   * with numeric coordinates.  For example, Projection("EPSG:4326") is the
   * system of representing positions with (longitude, latitude) pairs.
   */
  implicit class RichProjection(val crs: Projection) extends AnyVal {
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
    def to[G<:geometry.Geometry](dest: Projection)(g: G) = {
      val tx = CRS.findMathTransform(crs, dest.crs)
      JTS.transform(g, tx).asInstanceOf[G] 
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
}
