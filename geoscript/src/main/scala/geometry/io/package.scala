package org.geoscript.geometry

import org.geoscript.serialize._
import org.geotools.geojson.geom.GeometryJSON

object WKT extends Format[Geometry] {
  private val reader = new com.vividsolutions.jts.io.WKTReader()
  private val writer = new com.vividsolutions.jts.io.WKTWriter()
 
  def readFrom(in: java.io.Reader): Geometry = reader.read(in)

  def writeTo(out: java.io.Writer, geom: Geometry) { writer.write(geom, out) }
}

object WKB extends Codec[Geometry] {
  private val reader = new com.vividsolutions.jts.io.WKBReader()
  private val writer = new com.vividsolutions.jts.io.WKBWriter()
 
  def decodeFrom(in: java.io.InputStream): Geometry =
    reader.read(new com.vividsolutions.jts.io.InputStreamInStream(in))

  def encodeTo(out: java.io.OutputStream, g: Geometry) {
    writer.write(g, new com.vividsolutions.jts.io.OutputStreamOutStream(out))
  }
}

class GeoJSON(format: GeometryJSON) extends Format[Geometry] {
  def this(precision: Int) = this(new GeometryJSON(precision))

  def readFrom(source: java.io.Reader) = format.read(source)

  def writeTo(sink: java.io.Writer, g: Geometry): Unit =
    format.write(g, sink)
}

object GeoJSON extends GeoJSON(new GeometryJSON)

object GML extends Encoder[Geometry] {
  import org.geotools.xml.{ Parser, Encoder }
  import org.geotools.gml2

  def encodeTo(sink: java.io.OutputStream, g: Geometry) {
    val configuration = new gml2.GMLConfiguration
    val encoder = new Encoder(configuration)
    val nsUri = configuration.getNamespaceURI
    val qname = new javax.xml.namespace.QName(nsUri, g.getGeometryType)
    encoder.encode(g, qname, sink)
  }
}
