package org.geoscript.example 

object Render extends App {
  import org.geoscript.{ layer, style, render, projection },
    render.{ draw, png }

  def reference(e: org.geoscript.geometry.Envelope, p: projection.Projection) = 
    new org.geotools.geometry.jts.ReferencedEnvelope(e, p)

  val states = layer.Shapefile("../geoscript/src/test/resources/data/states.shp")
  val theme = style.CSS.fromFile("../geocss/src/test/resources/states.css")
  val bounds = reference(states.envelope, projection.Projection("EPSG:4326"))
  val win = new org.geoscript.render.Window
  draw(states, theme, Some(bounds), (1024, 1024), win)
}
