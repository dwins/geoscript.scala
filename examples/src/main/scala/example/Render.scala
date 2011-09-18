package org.geoscript.example 

import org.geoscript.layer.Shapefile,
  org.geoscript.style.CSS,
  org.geoscript.render.{ render, PNG, Viewport }

object Render {
  def main(args: Array[String]) {
    val states = Shapefile("geoscript/src/test/resources/data/states.shp")
    val theme = CSS.fromFile("geocss/src/test/resources/states.css")
    val frame = (1024, 1024) 
    val viewport = Viewport.pad(states.bounds, frame)
    render(
      viewport,
      Seq(states -> theme)
    ) on PNG("states.png", frame)
  }
}
