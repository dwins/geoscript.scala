package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}

/**
 * A number of implicit conversions for dealing with geometries.  GeoCrunch
 * users may prefer to simply extend <code>org.geoscript.GeoScript</code> which
 * collects implicits for many GeoScript objects.
 */
trait Implicits {
  implicit def tuple2coord(t: (Number, Number)): jts.Coordinate =
    new jts.Coordinate(t._1.doubleValue(), t._2.doubleValue())

  implicit def tuple2coord(t: (Number, Number, Number)): jts.Coordinate =
    new jts.Coordinate(
      t._1.doubleValue(), t._2.doubleValue(), t._3.doubleValue()
    )

  implicit def point2coord(p: jts.Point): jts.Coordinate = p.getCoordinate()

  implicit def enrichPoint(p: jts.Point): Point = Point(p)

  implicit def wrapGeom(geom: jts.Geometry): Geometry = Geometry(geom)

  implicit def unwrapGeom(geom: Geometry): jts.Geometry = geom.underlying
}
