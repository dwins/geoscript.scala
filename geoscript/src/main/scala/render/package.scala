package org.geoscript

import collection.JavaConverters._
import org.{ geotools => gt }
import gt.geometry.jts.ReferencedEnvelope
import java.awt
import java.awt.RenderingHints,
  RenderingHints.{ KEY_ANTIALIASING, VALUE_ANTIALIAS_ON }

package render {
  trait Stylable[T] {
    def applyStyle(t: T, s: style.Style): Layer
  }
  
  object Stylable {
    def stylable[T](f: (T, style.Style) => Layer): Stylable[T] =
      new Stylable[T] { def applyStyle(t: T, s: style.Style): Layer = f(t, s) }

    implicit val vectorDataIsStylable: Stylable[layer.Layer] =
      stylable { (l, s) => new gt.map.FeatureLayer(l, s.underlying, l.name) }
  }

  trait Canvas[T] { self =>
    def render(draw: Draw): T

    def map[U](f: T => U): Canvas[U] = new Canvas[U] {
      def render(draw: Draw): U =
        f(self.render(draw))
    }
  }

  class ImageCanvas extends Canvas[awt.image.BufferedImage] {
    val size = (256, 256) // TODO: Make this a constructor argument
    override def render(draw: Draw): awt.image.BufferedImage = {
      val (w, h) = size
      val img = new java.awt.image.BufferedImage(w, h, 
        java.awt.image.BufferedImage.TYPE_INT_ARGB)

      val graphics = img.getGraphics().asInstanceOf[java.awt.Graphics2D]
      graphics.setColor(java.awt.Color.WHITE)
      graphics.fillRect(0, 0, w, h)

      draw(graphics, size)
      graphics.dispose()
      img
    }
  }

  class Window extends Canvas[javax.swing.JFrame] {
    private val component = new javax.swing.JComponent {
      // createBufferStrategy(2)
      setBackground(java.awt.Color.WHITE)
      override def paintComponent(g: awt.Graphics) =
        draw(g.asInstanceOf[awt.Graphics2D], dimensionAsTuple(getSize()))
    }
    private val frame = new javax.swing.JFrame
    // frame.setResizable(false)
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(component)
    frame.setSize(256, 256)

    private var draw: Draw = { (_, _) => }

    override def render(draw: Draw): javax.swing.JFrame = {
      this.draw = draw
      component.repaint()
      if (!frame.isVisible) frame.setVisible(true)
      frame
    }
  }

  object Content {
    def empty = new Content
    def apply[T : Stylable](t: T, s: style.Style): Content =
      empty.withData(t, s)
  }

  class RichContent(content: Content) {
    def withLayer(l: Layer): Content = {
      val newContent = new Content
      newContent.layers.addAll(content.layers)
      newContent.addLayer(l)
      newContent
    }
    def withData[T : Stylable](data: T, s: style.Style): Content =
      withLayer(applyStyle(data, s))
  }
}

package object render {
  type Content = gt.map.MapContent
  type Draw = (awt.Graphics2D, Dimension) => Unit
  type Dimension = (Int, Int)
  type Layer = gt.map.Layer
  type StyleLayer = gt.map.Layer
  type DirectLayer = gt.map.DirectLayer

  def applyStyle[T : Stylable](t: T, s: org.geoscript.style.Style): StyleLayer =
    implicitly[Stylable[T]].applyStyle(t, s)

  def draw[Out](
    content: Content,
    // layers: Seq[Layer],
    bounds: ReferencedEnvelope,
    canvas: Canvas[Out] = new ImageCanvas
  ): Out = {
    // val content = new gt.map.MapContent
    // layers.foreach { content.addLayer }
    // 
    val hints = renderHints(KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON)

    canvas.render { (graphics, screenArea) =>
      val renderer = new gt.renderer.lite.StreamingRenderer()
      renderer.setJava2DHints(hints)
      renderer.setMapContent(content)
      renderer.paint(graphics, tupleAsRectangle(screenArea), bounds)
    }
  }
    
  def file(f: String) = new java.io.File(f)

  def png(f: java.io.File): Canvas[java.io.File] = 
    new ImageCanvas().map { i =>
      javax.imageio.ImageIO.write(i, "png", f)
      f
    }

  def png(f: String): Canvas[java.io.File] = png(file(f))

  private def renderHints(kv: (RenderingHints.Key, Any)*) = 
    new RenderingHints(kv.toMap.asJava)

  def dimensionAsTuple(dim: awt.Dimension): (Int, Int) = (dim.width, dim.height)
  def tupleAsRectangle(dim: (Int, Int)): awt.Rectangle = 
    new awt.Rectangle(0, 0, dim._1, dim._2)
}
