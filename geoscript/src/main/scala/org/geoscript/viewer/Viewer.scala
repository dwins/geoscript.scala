package org.geoscript.viewer

import org.geoscript.geometry._
import java.awt
import com.vividsolutions.jts.geom.{
  Envelope, GeometryFactory, MultiPolygon, Polygon
}
import org.geotools.geometry.jts.LiteShape
import swing.Swing._
import math._

/**
 * The Viewer object provides some rudimentary methods for rendering
 * geometries on-screen.
 */
object Viewer {
  /**
   * Draw the given Geometry in a window with default size.
   */
  def draw(g: Geometry) { draw(List(g), (500, 500)) } 

  /**
   * Draw the given Geometry in a window of the specified size (width, height).
   */
  def draw(g: Geometry, size: (Int, Int)) { draw(List(g), size) }
  
  /**
   * Draw all of the given Geometries in a window with default size 
   * (width, height).
   */
  def draw(gs: Seq[Geometry]) { draw(gs, (500, 500)) }

  /**
   * Draw all of the given Geometries in a window with the default size 
   * (height, width)
   */
  def draw(gs: Seq[Geometry], size: (Int, Int)) {
    val envelope = new Envelope
    for (geom <- gs) { envelope.expandToInclude(geom.bounds) }

    val scale =
      min({
          if (envelope.getWidth > 0) 
            size._1 / envelope.getWidth 
          else 
            java.lang.Double.MAX_VALUE
        }, {
          if (envelope.getHeight > 0) 
            size._2 / envelope.getHeight 
          else 
            1d
      })

    val tx = -envelope.getMinX
    val ty = -envelope.getMinY
    val at = new awt.geom.AffineTransform
    at.scale(scale, -scale)
    at.translate(tx, ty)
    at.translate(0, -(size._2 / scale))
    at.translate(50 / scale, -50 / scale)

    object Panel extends swing.Panel {
      override def paint(graphics: awt.Graphics2D) {
        val opaque = graphics.getComposite() 
        graphics.setRenderingHint(
          awt.RenderingHints.KEY_ANTIALIASING,
          awt.RenderingHints.VALUE_ANTIALIAS_ON
        )
        graphics.setStroke(new awt.BasicStroke(2))

        for (geom <- gs) {
          val shp = new LiteShape(geom.underlying, at, false)
          if (geom.isInstanceOf[Polygon] || geom.isInstanceOf[MultiPolygon]) {
            graphics.setColor(awt.Color.WHITE)
            graphics.setComposite(
              awt.AlphaComposite.getInstance(
                awt.AlphaComposite.SRC_OVER, 0.5f
              )
            )
            graphics.fill(shp)
          }

          graphics.setComposite(opaque)
          graphics.setColor(awt.Color.BLACK)
          graphics.draw(shp)
        }
      }
    }

    Panel.preferredSize = ((size._1 + 100, size._2 + 100): awt.Dimension)

    new swing.Frame {
      title = "GeoScript Viewer"
      contents = Panel
      pack
      visible = true
    }
  }
}
