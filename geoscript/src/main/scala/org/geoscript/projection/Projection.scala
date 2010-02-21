package org.geoscript.projection

import com.vividsolutions.jts.{geom => jts}

import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

class Projection(val crs: CoordinateReferenceSystem) {
  def to[Geom<:jts.Geometry](dest: Projection)(geom: Geom) = {
    val tx = CRS.findMathTransform(crs, dest.crs)
    JTS.transform(geom, tx).asInstanceOf[Geom] 
  }

  def id: String = CRS.toSRS(crs)
  def wkt: String = crs.toString()

  override def toString: String = 
    id match {
      case null => "<Unidentified projection>"
      case id => id
    }
}

object Projection {
  def apply(s: String): Projection = {
    new Projection(
      try {
        CRS.decode(s)
      } catch {
        case _ => CRS.parseWKT(s)
      }
    )
  }

  def apply(crs: CoordinateReferenceSystem) = new Projection(crs)
}

trait Implicits {
  val forceXY = System.getProperty("org.geotools.referencing.forceXY")

  if (forceXY == null || forceXY.toBoolean == false)
    System.setProperty("org.geotools.referencing.forceXY", "true")

  if (Hints.getSystemDefault(Hints.FORCE_LONGITUDE_FIRST_AXIS_ORDER)
      .asInstanceOf[Boolean])
    Hints.putSystemDefault(Hints.FORCE_AXIS_ORDER_HONORING, "http")

  implicit def enrich(crs: CoordinateReferenceSystem) = new RichCRS(crs)
  implicit def enrich(xform: MathTransform) = new RichTransform(xform)
}

class RichCRS(crs: CoordinateReferenceSystem) {
  def to(dest: CoordinateReferenceSystem): MathTransform = {
    CRS.findMathTransform(crs, dest)
  }
}

class RichTransform(tx: MathTransform) {
  def apply[Geom<:jts.Geometry](geom: Geom) =
    JTS.transform(geom, tx).asInstanceOf[Geom]
}
