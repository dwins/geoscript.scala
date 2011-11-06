package org.geoscript
package viewer

import geometry._, layer._, math._, render._, style._

import org.geotools.geometry.jts.LiteShape
import java.awt.{ Graphics2D, RenderingHints }
import scala.collection.JavaConversions._

private class MapWidget extends swing.Component {
  var viewport = Envelope(-180, -90, 180, 90)
  var layers: Seq[SpatialRenderable] = Nil

  override def paint(graphics: swing.Graphics2D) = {
    locally { import RenderingHints._
      graphics.setRenderingHints(new RenderingHints(Map(
        KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON,
        KEY_TEXT_ANTIALIASING -> VALUE_TEXT_ANTIALIAS_ON
      )))
    }
    import org.geoscript.render.Viewport.pad
    val displayBounds = pad(viewport, (bounds.width, bounds.height))
    render(pad(viewport), layers) on Direct(graphics, bounds)
  }
}

/**
 * The Viewer object provides some rudimentary methods for rendering
 * geospatial information on-screen.
 */
object Viewer {
  private var window: Option[(swing.Window, MapWidget)] = None

  def display(layers: Seq[SpatialRenderable]) {
    window match {
      case Some((frame, map)) =>
        map.layers = layers
        frame.repaint()
      case None =>
        swing.Swing.onEDT { 
          val frame = new swing.MainFrame()
          val mapViewer = new MapWidget()
          mapViewer.layers = layers
          layers.flatMap(_.definitionExtent).reduceOption(_ ** _).foreach {
            mapViewer.viewport = _
          }
          frame.visible = true
          frame.contents = mapViewer
          frame.size = new swing.Dimension(500, 500)
          Viewer.synchronized {
            window = Some((frame, mapViewer))
          }
        }
    }
  }
}
