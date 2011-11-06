package org.geoscript
package object geometry {
  import com.vividsolutions.jts.{geom => jts}

  type Geometry = jts.Geometry
  type GeometryCollection = jts.GeometryCollection
  type Polygon = jts.Polygon
  type MultiPolygon = jts.MultiPolygon
  type LineString = jts.LineString
  type MultiLineString = jts.MultiLineString
  type Point = jts.Point
  type MultiPoint = jts.MultiPoint
  
  type Coordinate = jts.Coordinate
  type Envelope = jts.Envelope

  val factory = new jts.GeometryFactory
}
