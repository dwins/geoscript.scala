package org.geoscript.example 

import org.geoscript.layer.Shapefile,
  org.geoscript.style.CSS,
  org.geoscript.render.{ render, PNG, Viewport },
  org.geoscript.projection.Projection

object Render {
  def reference(e: org.geoscript.geometry.Envelope, p: Projection) = 
    new org.geotools.geometry.jts.ReferencedEnvelope(e, p)

  def main(args: Array[String]) {
    val states = Shapefile("geoscript/src/test/resources/data/states.shp")
    val theme = CSS.fromFile("geocss/src/test/resources/states.css")
    val frame = (1024, 1024) 
    val viewport = Viewport.pad(reference(states.envelope, Projection("EPSG:4326")), frame)
    render(
      viewport,
      Seq(states -> theme)
    ) on PNG("states.png", frame)
  }
}
