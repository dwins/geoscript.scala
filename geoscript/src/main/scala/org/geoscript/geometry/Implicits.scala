package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}

trait Implicits {
  implicit def tuple2coord(t: (Number, Number)): jts.Coordinate =
    new jts.Coordinate(t._1.doubleValue(), t._2.doubleValue())

  implicit def tuple2coord(t: (Number, Number, Number)): jts.Coordinate =
    new jts.Coordinate(
      t._1.doubleValue(), t._2.doubleValue(), t._3.doubleValue()
    )

  implicit def point2coord(p: jts.Point): jts.Coordinate = p.getCoordinate()

  implicit def enrichPoint(p: jts.Point): RichPoint = 
    new RichPoint(p)

  implicit def enrichGeometry(g: jts.Geometry): RichGeometry =
    new RichGeometry(g)
}
