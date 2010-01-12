package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}

private object ModuleInternals {
  val factory = new jts.GeometryFactory()
}

import ModuleInternals.factory._

object Point {
  def apply(x: Double, y: Double, z: Double): jts.Point =
    createPoint(new jts.Coordinate(x, y, z))

  def apply(x: Double, y: Double): jts.Point = 
    createPoint(new jts.Coordinate(x, y))
}

object LineString {
  def apply(vertices: jts.Coordinate*): jts.LineString =
    createLineString(vertices.toArray)

  def apply(vertices: jts.CoordinateSequence): jts.LineString =
    createLineString(vertices)
}

object GeometryImplicits {
  implicit def enrich(p: jts.Point): RichPoint = new RichPoint(p)
}

class RichGeometry(geom: jts.Geometry) {
  def writeAs(enc: Encoder): String = enc.encode(geom)
  def & (g: jts.Geometry): Boolean = geom intersects g
  def && (g: jts.Geometry): jts.Geometry = geom intersection g
  def || (g: jts.Geometry): jts.Geometry = geom union g
  def simplify(threshold: Double): jts.Geometry = {
    com.vividsolutions.jts.simplify
      .TopologyPreservingSimplifier.simplify(geom, threshold)
  }
}

class RichPoint(p: jts.Point) extends RichGeometry(p) {
  override def && (g: jts.Geometry): jts.Point = (p intersection g).asInstanceOf[jts.Point]
  override def || (g: jts.Geometry): jts.Point = (p union g).asInstanceOf[jts.Point]
}

trait Encoder {
  def encode(g: jts.Geometry): String;
}

/**
 * A sample Encoder implementation demonstrating how to add output formats.
 * This one simply reuses the toString method from JTS.
 */
object WKT extends Encoder {
  def encode(g: jts.Geometry): String = g.toString() 
}
