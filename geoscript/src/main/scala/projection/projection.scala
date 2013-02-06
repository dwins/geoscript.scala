package org.geoscript

import org.geoscript.geometry._
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

package object projection {
  /**
   * A Projection is a particular system of representing real-world locations
   * with numeric coordinates.  This encompasses such details a decision of
   * units, an origin point, axis directions, and models of the earth's
   * curvature.  When performing geometric calculations, it is generally
   * necessary to ensure that they are using the same Projection in order to
   * get meaningful results.
   *
   * A [[org.geoscript.projection.Transform Transform]] may be used to convert geometry
   * representations between different Projections.  The
   * [[org.geoscript.projection.RichProjection RichProjection]] class provides some
   * convenience methods for operating with Projections.
   */
  type Projection = org.opengis.referencing.crs.CoordinateReferenceSystem
  
  /**
   * A Transform is a function for converting coordinates from one Projection to another.
   * @see [[org.geoscript.projection.Projection]]
   */
  type Transform = org.opengis.referencing.operation.MathTransform

  /**
   * Depending on the services involved, some projections may have the Y-axis
   * first (ie, use (Y,X) coordinate pairs instead of (X,Y)).  This method can
   * be called in order to always force projections to be interpreted in (X,Y)
   * order.  Otherwise, the treatment of axis order will be determined by the
   * org.geotools.referencing.forceXY system property, or false by default.
   *
   * @note that this method should be called before constructing any Projection
   * objects - Projections constructed before forcing XY mode may have their
   * axes flipped.
   */
  def forceXYMode() {
    import org.geotools.factory.Hints

    val forceXY = System.getProperty("org.geotools.referencing.forceXY")

    if (forceXY == null || forceXY.toBoolean == false)
      System.setProperty("org.geotools.referencing.forceXY", "true")

    if (Hints.getSystemDefault(Hints.FORCE_LONGITUDE_FIRST_AXIS_ORDER)
        .asInstanceOf[Boolean])
      Hints.putSystemDefault(Hints.FORCE_AXIS_ORDER_HONORING, "http")
  }
  
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
   * LatLon is a Projection that interprets coordinates as Latitude/Longitude
   * pairs.
   */
  def LatLon = lookupEPSG("EPSG:4326").get

  /**
   * WebMercator is a Projection that corresponds to many commercial tile sets
   * and is commonly used with web mapping APIs.
   */
  def WebMercator = lookupEPSG("EPSG:3857").get

  def reproject[T](t: T, p: Projection)(implicit ev: HasProjection[T]): T =
    ev.reproject(t, p)

  implicit class RichProjection(val crs: Projection) extends AnyVal {
    /**
     * Create a Transform from this projection to another one, which
     * can be applied to JTS Geometries. Example usage:
     * 
     * {{{
     * import org.geoscript._, geometry.builder._, projection._
     * val point = Point(1, 2)
     * val convert = LatLon to WebMercator
     * val reprojected = convert(point)
     * }}}
     */
    def to(dest: Projection): Transform = CRS.findMathTransform(crs, dest)

    /**
     * Get the official spatial reference identifier (SRID) for this projection, if any
     */
    def id: String = CRS.toSRS(crs)

    /**
     * Get the Well Known Text specification of this projection.
     * 
     * @see http://en.wikipedia.org/wiki/Well-known_text
     */
    def wkt: String = crs.toString()
  }

  implicit class RichTransform(val transform: Transform) extends AnyVal {
    def apply[G <: Geometry](geometry: G): G =
      JTS.transform(geometry, transform).asInstanceOf[G]
  }
}

package projection {
  trait HasProjection[T] {
    def reproject(t: T, projection: Projection): T
  }
}
