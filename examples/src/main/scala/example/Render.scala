package org.geoscript.example 

import org.geoscript._
import layer.Shapefile, style.CSS, render._

object Render {
  def main(args: Array[String]) {
    val states = Shapefile("geoscript/src/test/resources/data/states.shp")
    val theme = CSS.fromFile("geocss/src/test/resources/states.css")
    val viewport = Viewport(Viewport.pad(states.bounds, ((8.5 * 72).toInt, 11 * 72)))
    val frame = (8.5f * 72, 11f * 72)
    println(frame)
    viewport.render(Seq(states -> theme))
      .on(PDF("states.pdf", frame))
    // viewer.Viewer.display(Seq(states -> theme))
  }
}
