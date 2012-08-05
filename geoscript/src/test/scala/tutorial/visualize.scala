package tutorial

import org.geoscript._, render._
import org.geotools.geometry.jts.ReferencedEnvelope

object Visualize extends App {
  val poly = geometry.polygon(
    Seq((35,10), (10,20), (15,40), (45,45), (35,10)),
    Seq(Seq((20,30), (35,35), (30,20), (20,30))))
  val bounds = new ReferencedEnvelope(poly.getEnvelopeInternal, projection.LatLon)
  val window = new Window
  draw(Content(poly), Stretch(bounds), window)
}
