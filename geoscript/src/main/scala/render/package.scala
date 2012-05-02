package org.geoscript

import collection.JavaConverters._
import org.{ geotools => gt }
import gt.geometry.jts.ReferencedEnvelope
import java.awt
import java.awt.RenderingHints,
  RenderingHints.{ KEY_ANTIALIASING, VALUE_ANTIALIAS_ON }

package render {
  trait Stylable[T] {
    def applyStyle(t: T, s: style.Style): gt.map.Layer
    def boundsOf(t: T): ReferencedEnvelope
  }
  
  object Stylable {
    implicit val vectorDataIsStylable: Stylable[layer.Layer] =
      new Stylable[layer.Layer] {
        override def applyStyle(lyr: layer.Layer, s: style.Style)
        : gt.map.Layer =
          new gt.map.FeatureLayer(lyr.source, s.underlying, lyr.name)

        override def boundsOf(lyr: layer.Layer): ReferencedEnvelope =
          new ReferencedEnvelope(lyr.envelope, lyr.schema.geometry.projection)
      }
  }

  trait Canvas[T] { self =>
    def render(size: (Int, Int), draw: awt.Graphics2D => Unit): T

    def map[U](f: T => U): Canvas[U] = new Canvas[U] {
      def render(size: (Int, Int), draw: awt.Graphics2D => Unit): U =
        f(self.render(size, draw))
    }
  }

  class ImageCanvas extends Canvas[awt.image.BufferedImage] {
    def render(
      size: (Int, Int), draw: awt.Graphics2D => Unit
    ): awt.image.BufferedImage = {
      val (w, h) = size
      val img = new java.awt.image.BufferedImage(w, h, 
        java.awt.image.BufferedImage.TYPE_INT_ARGB)

      val graphics = img.getGraphics().asInstanceOf[java.awt.Graphics2D]
      graphics.setColor(java.awt.Color.WHITE)
      graphics.fillRect(0, 0, w, h)

      draw(graphics)
      graphics.dispose()
      img
    }
  }

  class Window extends Canvas[javax.swing.JFrame] {
    private val component = new java.awt.Canvas {
      setBackground(java.awt.Color.WHITE)
      override def paint(g: awt.Graphics) =
        draw(g.asInstanceOf[awt.Graphics2D])
    }
    private val frame = new javax.swing.JFrame
    frame.setResizable(false)
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(component)

    private var draw: awt.Graphics2D => Unit = { _ => }

    def render(
      size: (Int, Int), draw: awt.Graphics2D => Unit
    ): javax.swing.JFrame = {
      frame.setSize(size._1, size._2)
      this.draw = draw
      component.repaint()
      if (!frame.isVisible) frame.setVisible(true)
      frame
    }
  }
}

package object render {
  def draw[T, Out](
    t: T,
    sty: style.Style,
    bounds: Option[ReferencedEnvelope] = None,
    size: (Int, Int) = (512, 512),
    canvas: Canvas[Out] = new ImageCanvas
  ) (
    implicit ev: Stylable[T]
  ): Out
  = {
    val effectiveBounds = bounds.getOrElse(ev.boundsOf(t))

    val context = new gt.map.DefaultMapContext
    context.setAreaOfInterest(effectiveBounds)
    context.addLayer(ev.applyStyle(t, sty))

    val hints = renderHints(KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON)
    val renderer = new gt.renderer.lite.StreamingRenderer()
    renderer.setJava2DHints(hints)
    renderer.setContext(context)
    val (w, h) = size
    canvas.render(size, { g =>
      renderer.paint(g, new java.awt.Rectangle(w, h), effectiveBounds)
    })
  }

  def file(f: String) = new java.io.File(f)

  def png(f: java.io.File): Canvas[java.io.File] = 
    new ImageCanvas().map { i =>
      javax.imageio.ImageIO.write(i, "png", f)
      f
    }

  def png(f: String): Canvas[java.io.File] = png(file(f))

  private def renderHints(kv: (RenderingHints.Key, Any)*) = 
    new RenderingHints(Map(kv: _*).asJava)
}
