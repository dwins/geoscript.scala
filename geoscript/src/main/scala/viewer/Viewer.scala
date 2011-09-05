package org.geoscript
package viewer

import geometry._, layer._, render._, style._

import org.geotools.geometry.jts.LiteShape
import math._

private class MapWidget extends swing.Component {
  var viewport: Viewport = Viewport(Bounds(-180, -90, 180, 90) in "EPSG:4326")
  var layers: Seq[(Layer, Style)] = Nil

  override def paint(graphics: swing.Graphics2D) {
    viewport.render(layers).on(Direct(graphics, bounds))
  }
}

/**
 * The Viewer object provides some rudimentary methods for rendering
 * geospatial information on-screen.
 */
object Viewer {
  private var window: Option[(swing.Window, MapWidget)] = None

  def display(layers: Seq[(Layer, Style)]) {
    window match {
      case Some((frame, map)) =>
        map.layers = layers
        frame.repaint()
      case None =>
        swing.Swing.onEDT { 
          val frame = new swing.Frame()
          val mapViewer = new MapWidget()
          mapViewer.layers = layers
          mapViewer.viewport = Viewport(layers.head._1.bounds) // TODO: Put a sweet fold here
          frame.size = new swing.Dimension(500, 500)
          frame.visible = true
          frame.contents = mapViewer
          Viewer.synchronized {
            window = Some((frame, mapViewer))
          }
        }
    }
  }
}
