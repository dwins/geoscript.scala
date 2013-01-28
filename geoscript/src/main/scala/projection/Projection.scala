package org.geoscript.projection

import com.vividsolutions.jts.{geom => jts}

import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

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
  def apply(s: String): Projection = (lookupEPSG(s) orElse parseWKT(s)).get
}
