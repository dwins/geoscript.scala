package org.geoscript

import scala.util.control.Exception.catching 

import com.vividsolutions.jts.{geom => jts}

import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform
import org.opengis.referencing.{ FactoryException, NoSuchAuthorityCodeException }

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

package object projection {
  type Projection = CoordinateReferenceSystem
  type Transform = org.opengis.referencing.operation.MathTransform

  private val catchLookup = catching(classOf[FactoryException])

  def fromWKT(s: String): Option[Projection] =
    catching(classOf[FactoryException]).opt { CRS.parseWKT(s) }

  def fromSrid(s: String): Option[Projection] = 
    catching(classOf[FactoryException], classOf[NoSuchAuthorityCodeException]).opt {
       CRS.decode(s)
    }

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

package projection {
  class RichProjection(p: Projection) {
    def id = CRS.lookupIdentifier(p, true)
  }
}
