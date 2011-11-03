package org.geoscript.feature

@annotation.implicitNotFound(
  "No geometry representation found for ${G}"
)
trait BoundGeometry[G] {
  def binding: Class[_]
}

object BoundGeometry {
  import com.vividsolutions.jts.{ geom => jts }

  private def bind[G](c: Class[G]): BoundGeometry[G] =
    new BoundGeometry[G] { def binding = c }

  implicit val boundPoint = bind(classOf[jts.Point])
  implicit val boundMultiPoint = bind(classOf[jts.MultiPoint])
  implicit val boundLineString = bind(classOf[jts.LineString])
  implicit val boundMultiLinestring = bind(classOf[jts.MultiLineString])
  implicit val boundPolygon = bind(classOf[jts.Polygon])
  implicit val boundMultiPolygon = bind(classOf[jts.MultiPolygon])
  implicit val boundGeometry = bind(classOf[jts.Geometry])
  implicit val boundGeometryCollection = bind(classOf[jts.GeometryCollection])
}

@annotation.implicitNotFound(
  "No scalar representation found for ${S} (did you forget the projection for your geometry?)"
)
trait BoundScalar[S] {
  def binding: Class[_]
}

object BoundScalar {
  private def bind[S](c: Class[S]): BoundScalar[S] =
    new BoundScalar[S] { def binding = c }

  implicit val boundBoolean = bind(classOf[java.lang.Boolean])
  implicit val boundByte = bind(classOf[java.lang.Byte])
  implicit val boundShort = bind(classOf[java.lang.Short])
  implicit val boundInteger = bind(classOf[java.lang.Integer])
  implicit val boundLong = bind(classOf[java.lang.Long])
  implicit val boundFloat = bind(classOf[java.lang.Float])
  implicit val boundDouble = bind(classOf[java.lang.Double])
  implicit val boundString = bind(classOf[String])
  implicit val boundDate = bind(classOf[java.util.Date])
}
