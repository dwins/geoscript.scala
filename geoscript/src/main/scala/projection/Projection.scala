package org.geoscript

import com.vividsolutions.jts.{geom => jts}

import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

package object projection {
  type Projection = CoordinateReferenceSystem
  type Transform = org.opengis.referencing.operation.MathTransform

  def fromWKT(s: String): Option[Projection] = Some(CRS.parseWKT(s))
  def fromSrid(s: String): Option[Projection] = Some(CRS.decode(s))
  def reproject[G : Projectable]
    (p: Projection, q: Projection)
    (g: G)
  : G = implicitly[Projectable[G]].project(p, q)(g)

  def transform(p: Projection, q: Projection): Transform =
    CRS.findMathTransform(p, q)

  def Projection(s: String): Projection = (fromSrid(s) orElse fromWKT(s)).orNull

  lazy val LatLon = fromSrid("EPSG:4326").get
  lazy val WebMercator = fromSrid("EPSG:3857").get

  def reference[T : Projectable](t: T, proj: Projection): Referenced[T] =
    Referenced(t, proj)

  def aspatial[T](t: T): Referenced[T] = Referenced(t)
}
