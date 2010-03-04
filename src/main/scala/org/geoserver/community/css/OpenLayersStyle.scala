package org.geoserver.community.css

import org.geotools.{ styling => gt }

object OpenLayersStyle {
  def write(style: gt.Style, out: java.io.OutputStream) {
    val writer = new java.io.OutputStreamWriter(out)
    writer.write(
      "var styleMap = new OpenLayers.StyleMap(OpenLayers.Util.applyDefaults("
    )
    writer.write("{}")
    writer.write(",")
    writer.write("OpenLayers.Feature.Vector.style[\"default\"]")
    writer.write(");")
    writer.flush()
    writer.close()
  }
}
