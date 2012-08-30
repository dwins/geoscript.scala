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

  lazy val LatLon = fromSrid("EPSG:4326").get
  lazy val WebMercator = fromSrid("EPSG:3857").get

  private val catchLookup = catching(classOf[FactoryException])

  def forceXYAxisOrder() {
    System.setProperty("org.geotools.referencing.forceXY", "true")
  }

  def fromWKT(s: String): Option[Projection] =
    catching(classOf[FactoryException]).opt { CRS.parseWKT(s) }

  def fromSrid(s: String): Option[Projection] = 
    catching(classOf[FactoryException], classOf[NoSuchAuthorityCodeException]).opt {
       CRS.decode(s)
    }

  // NOTE: Some data types store projections but not coordinates (eg, Schema).
  // For these, force and reproject are the same.

  // Update projection tracking information without modifying coordinates
  def force[G : HasProjection](proj: Projection, g: G): G =
    implicitly[HasProjection[G]].setProjection(proj)(g)

  // Reproject coordinate data and update projection tracking informationo
  def reproject[G : HasProjection](proj: Projection, g: G): G =
    implicitly[HasProjection[G]].transform(proj)(g)

  def transform[G : Projectable](source: Projection, dest: Projection, g: G): G =
    implicitly[Projectable[G]].project(source, dest)(g)

  def lookupTransform(p: Projection, q: Projection): Transform =
    CRS.findMathTransform(p, q)

  def Projection(s: String): Projection = (fromSrid(s) orElse fromWKT(s)).orNull
}

package projection {
  import org.geoscript.serialize._

  class RichProjection(p: Projection) {
    def id: Option[String] = Option(CRS.lookupIdentifier(p, true))
  }

  object WKT extends Format[Projection] {
    def readFrom(in: java.io.Reader): Projection = {
      val accum = new StringBuilder()
      val buff = Array.ofDim[Char](4096)
      var len = 0
      while ({ len = in.read(buff) ; len >= 0 }) {
        accum.appendAll(buff, 0, len)
      }
      CRS.parseWKT(accum.toString)
    }

    def writeTo(out: java.io.Writer, p: Projection): Unit = {
      out.write(p.toWKT)
    }
  }
}
