package org.geoscript.geometry
package io

import org.geoscript.io.{ Sink, Source }

trait Reader[T] {
  def read(source: Source): T
}

trait Writer[T] {
  def write[U](t: T, sink: Sink[U]): U
}

trait Format[T] extends Writer[T] with Reader[T] 

object WKT extends Format[Geometry] {
  private val reader = new com.vividsolutions.jts.io.WKTReader()
  private val writer = new com.vividsolutions.jts.io.WKTWriter()
 
  def read(source: Source): Geometry = 
    source { in =>
      val chars = new java.io.InputStreamReader(in)
      val res = reader.read(chars)
      chars.close()

      res
    }

  def write[T](geom: Geometry, sink: Sink[T]): T =
    sink { out =>
      val chars = new java.io.OutputStreamWriter(out)
      writer.write(geom, chars)
      chars.close()
    }
}

object WKB extends Format[Geometry] {
  private val reader = new com.vividsolutions.jts.io.WKBReader()
  private val writer = new com.vividsolutions.jts.io.WKBWriter()
 
  def read(source: Source): Geometry = 
    source { in =>
      val s = new com.vividsolutions.jts.io.InputStreamInStream(in)
      reader.read(s)
    }

  def write[T](geom: Geometry, sink: Sink[T]): T =
    sink { out =>
      val s = new com.vividsolutions.jts.io.OutputStreamOutStream(out)
      writer.write(geom, s)
    }
}

import org.geotools.geojson.geom.GeometryJSON
class GeoJSON(format: GeometryJSON) extends Format[Geometry] {
  def this(precision: Int) = this(new GeometryJSON(precision))
  def read(source: Source): Geometry = source { format.read(_) }
  def write[T](g: Geometry, sink: Sink[T]): T = sink { format.write(g, _) }
}

object GeoJSON extends GeoJSON(new GeometryJSON)

import org.geotools.xml.{ Parser, Encoder }
import org.geotools.gml2

object GML extends Writer[Geometry] {
  def write[T](g: Geometry, sink: Sink[T]): T = {
    val configuration = new gml2.GMLConfiguration
    val encoder = new Encoder(configuration)
    val nsUri = configuration.getNamespaceURI
    val qname = new javax.xml.namespace.QName(nsUri, g.getGeometryType)
    sink { encoder.encode(g, qname, _) }
  }
}
