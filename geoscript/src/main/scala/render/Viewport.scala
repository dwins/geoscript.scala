package org.geoscript

import collection.JavaConversions._
import com.vividsolutions.jts.{geom=>jts}
import java.awt.{ Graphics2D, Rectangle, RenderingHints }

package render {
  trait Context[T] {
    def apply(op: (Graphics2D, Rectangle) => Unit): T
  }

  case class Direct(graphics: Graphics2D, bounds: Rectangle) extends Context[Unit] {
    def apply(op: (Graphics2D, Rectangle) => Unit): Unit = op(graphics, bounds)
  }

  case class Raster(area: Rectangle) extends Context[java.awt.image.BufferedImage] {
    def apply(draw: (Graphics2D, Rectangle) => Unit): java.awt.image.BufferedImage = {
      import java.awt.image.BufferedImage
      val image = new BufferedImage(
        area.width,
        area.height,
        BufferedImage.TYPE_INT_RGB
      )
      val graphics = image.createGraphics()
      draw(graphics, area)
      graphics.dispose()
      image
    }
  }

  trait Streamable[T] { 
    def apply(op: java.io.OutputStream => Unit): T
  }

  object Streamable {
    implicit def stream(out: java.io.OutputStream): Streamable[Unit] =
      new Streamable[Unit] { 
        def apply(op: java.io.OutputStream => Unit) = op(out)
      }

    implicit def file(name: String): Streamable[java.io.File] =
      file(new java.io.File(name))

    implicit def file(file: java.io.File): Streamable[java.io.File] =
      new Streamable[java.io.File] { 
        def apply(op: java.io.OutputStream => Unit) = {
          val output = new java.io.BufferedOutputStream(new java.io.FileOutputStream(file))
          stream(output)(op)
          output.close()

          file
        }
      }

    def buffer: Streamable[Array[Byte]] =
      new Streamable[Array[Byte]] {
        def apply(op: java.io.OutputStream => Unit) = {
          val output = new java.io.ByteArrayOutputStream
          stream(output)(op)
          output.close()
          output.toByteArray
        }
      }
  }

  trait Renderable extends ((Graphics2D, Rectangle) => Unit) {
    // hm, this seems a bit gimmicky.
    def on[T](context: Context[T]): T = context(this)
  }

  object Renderable {
    def apply(op: (Graphics2D, Rectangle) => Unit) = 
      new Renderable {
        def apply(g: Graphics2D, r: Rectangle) = op(g, r)
      }
  }

  case class Viewport(bounds: geometry.Bounds) {
    private def draw(
      graphics: java.awt.Graphics2D,
      layers: Seq[(layer.Layer, style.Style)],
      window: java.awt.Rectangle
    ) {
      import RenderingHints._
      val renderer = new org.geotools.renderer.lite.StreamingRenderer()
      renderer.setJava2DHints(new RenderingHints(Map(
        KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON,
        KEY_TEXT_ANTIALIASING -> VALUE_TEXT_ANTIALIAS_ON
      )))
      val context = new org.geotools.map.DefaultMapContext()
      for ((data, style) <- layers) {
        context.addLayer(
          new org.geotools.map.FeatureLayer(data.source, style.underlying)
        )
      }
      renderer.setContext(context)
      renderer.paint(graphics, window, bounds)
      context.dispose()
    }

    def render(layers: Seq[(layer.Layer, style.Style)]): Renderable =
      Renderable { draw(_, layers, _) } 
  }

  object Viewport {
    /**
     * From a list of layers and a desired screen size, determine the size of
     * viewing area that preserves the aspect ratio of the layers' bounds and
     * fits within the given size.
     */
    def frame(
      envelope: geometry.Bounds,
      maximal: (Int, Int) = (500, 500)
    ): java.awt.Rectangle = {
      val aspect = envelope.height / envelope.width
      val idealAspect = maximal._2.toDouble / maximal._1.toDouble
      if (aspect < idealAspect) {
        new java.awt.Rectangle(0, 0, maximal._1, (maximal._2 * aspect).toInt)
      } else {
        new java.awt.Rectangle(0, 0, (maximal._1 / aspect).toInt, maximal._2)
      }
    }

    /** 
     * From a real-world envelope and a Rectangle representing the display
     * area, expand the envelope so that it matches the aspect ratio of the
     * desired viewing window.  The center of the envelope is preserved.
     */
    def pad(envelope: geometry.Bounds, window: (Int, Int) = (500, 500))
    : geometry.Bounds = {
      val aspect = envelope.height / envelope.width
      val idealAspect = window._2 / window._1
      val padded = (
        if (aspect < idealAspect) {
          val height = envelope.height * (idealAspect/aspect)
          geometry.Bounds(
            envelope.minX, envelope.centre.y - height/2,
            envelope.maxX, envelope.centre.y + height/2
          )
        } else {
          val width = envelope.width * (aspect/idealAspect)
          geometry.Bounds(
            envelope.centre.x - width/2, envelope.minY,
            envelope.centre.x + width/2, envelope.maxY
          )
        }
      )
      if (envelope.projection != null)
        padded in envelope.projection
      else padded
    }
  }
}

package object render {
  def PNG[T](sink: Streamable[T], window: Rectangle = new Rectangle(0, 0, 500, 500)): Context[T] =
    new Context[T] { 
      def apply(draw: (Graphics2D, Rectangle) => Unit): T = {
        import java.awt.image.BufferedImage
        val image = new BufferedImage(
          window.width,
          window.height,
          BufferedImage.TYPE_INT_ARGB
        )
        val graphics = image.createGraphics()
        draw(graphics, window)
        graphics.dispose()
        sink { out => javax.imageio.ImageIO.write(image, "PNG", out) }
      }
    }

  def JPEG[T](sink: Streamable[T], window: Rectangle = new Rectangle(0, 0, 500, 500)): Context[T] =
    new Context[T] { 
      def apply(draw: (Graphics2D, Rectangle) => Unit): T = {
        import java.awt.image.BufferedImage
        val image = new BufferedImage(
          window.width,
          window.height,
          BufferedImage.TYPE_INT_RGB
        )
        val graphics = image.createGraphics()
        draw(graphics, window)
        graphics.dispose()
        sink { out => javax.imageio.ImageIO.write(image, "JPEG", out) }
      }
    }

  def GIF[T](sink: Streamable[T], window: Rectangle = new Rectangle(0, 0, 500, 500)): Context[T] =
    new Context[T] { 
      def apply(draw: (Graphics2D, Rectangle) => Unit): T = {
        import java.awt.image.BufferedImage
        val image = new BufferedImage(
          window.width,
          window.height,
          BufferedImage.TYPE_INT_RGB
        )
        val graphics = image.createGraphics()
        draw(graphics, window)
        graphics.dispose()
        sink { out => javax.imageio.ImageIO.write(image, "GIF", out) }
      }
    }

  def PDF[T](sink: Streamable[T],
    pageDimensions: (Float, Float) = (8.5f * 72, 11f * 72),
    dpi: Float = 96f
  ): Context[T] =
    new Context[T] {
      import com.lowagie.{ text => itext } 
      def apply(draw: (Graphics2D, Rectangle) => Unit): T = {
        sink { output => 
            val (width, height) = pageDimensions
          val document =
            new itext.Document(new itext.Rectangle(width + 72, height + 72))
          document setMargins(0,0,0,0) // margins in pt, divide by 72 for inches 
          val writer = itext.pdf.PdfWriter.getInstance(document, output)
          val mapper = new itext.pdf.DefaultFontMapper
          document.open()

          val content = writer.getDirectContent()
          val template = content.createTemplate(width, height)
          val graphic = template.createGraphics(width, height, mapper)

          val paintArea = new java.awt.Rectangle(0, 0, width.toInt, height.toInt)

          // graphic.clipRect(36, 36, width.toInt, height.toInt)
          // graphic.translate(36, 36)
          draw(graphic, paintArea)
          graphic.dispose()
          content.addTemplate(template, 0, 0)
          document.close()
        }
      }
    }
}
