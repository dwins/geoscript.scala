package org.geoscript

import com.vividsolutions.jts.{geom => jts}

import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

package object projection {
  type Projection = CoordinateReferenceSystem

  def fromWKT(s: String): Option[Projection] = Some(CRS.parseWKT(s))
  def fromSrid(s: String): Option[Projection] = Some(CRS.decode(s))
  def transform[G <: geometry.Geometry]
    (p: Projection, q: Projection)
    (g: G)
  : G = sys.error("Unimplemented")

  def Projection(s: String): Projection = (fromSrid(s) orElse fromWKT(s)).orNull

  lazy val LatLon = fromSrid("EPSG:4326")
  lazy val WebMercator = fromSrid("EPSG:3857")
}
