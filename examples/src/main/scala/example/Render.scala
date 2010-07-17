package org.geoscript.example 

import org.geoscript._

object Render {
  def main(args: Array[String]) {
    val states = layer.Shapefile("geoscript/src/test/resources/data/states.shp")
    val theme = style.CSS.load("geocss/src/test/resources/states.css")
    val viewport = render.Viewport(states.bounds)
    viewport.render(Seq(states -> theme))
  }
}
