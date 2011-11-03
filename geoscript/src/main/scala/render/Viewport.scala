package org.geoscript

import io._

import collection.JavaConversions._
import com.vividsolutions.jts.{geom=>jts}
import java.awt.{ Graphics2D, Rectangle, RenderingHints }
import geometry.Bounds

package render {
  trait Context[T] {
    def apply(op: (Graphics2D, Rectangle) => Unit): T
  }

  case class Direct(graphics: Graphics2D, window: Rectangle) extends Context[Unit] {
    def apply(op: (Graphics2D, Rectangle) => Unit): Unit =
      op(graphics, window)
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

  trait Renderable extends ((Graphics2D, Rectangle) => Unit) {
    // hm, this seems a bit gimmicky.
    def on[T](context: Context[T]): T = context(this)
  }

  object Renderable {
    def apply(op: (Graphics2D, Rectangle) => Unit) = 
      new Renderable {
        def apply(graphics: Graphics2D, window: Rectangle) =
          op(graphics, window)
      }

    def chain(renderables: Seq[Renderable]) = Renderable { (g, w) =>
      renderables.foreach(_ apply (g, w))
    }
  }

  trait SpatialRenderable extends (Bounds => Renderable) {
    def definitionExtent: Option[Bounds]
  }

  object SpatialRenderable {
    implicit def fromGeometry(g: geometry.Geometry) =
      new SpatialRenderable {
        def definitionExtent: Option[Bounds] = Some(g.bounds)
        def apply(bounds: Bounds) =
          Renderable { (graphics, window) =>
            import geometry.Transform
            val tx = Transform
              .translate(-bounds.minX, -bounds.minY)
              .scale(window.width / bounds.width.toDouble, window.height / bounds.height.toDouble)
              .translate(0, -window.height)
              .scale(1, -1)
            graphics.draw(new org.geotools.geometry.jts.LiteShape(tx(g).underlying, null, false))
          }
      }

    implicit def fromStyledLayer(t: (layer.Layer, style.Style)) =
      new SpatialRenderable {
        val (layer, style) = t
        def definitionExtent: Option[Bounds] = Some(layer.bounds)
        def apply(bounds: Bounds): Renderable = 
          Renderable { (graphics, window) =>
            val renderer = new org.geotools.renderer.lite.StreamingRenderer()
            locally { import RenderingHints._
              renderer.setJava2DHints(new RenderingHints(Map(
                KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON,
                KEY_TEXT_ANTIALIASING -> VALUE_TEXT_ANTIALIAS_ON
              )))
            }
            val content = new org.geotools.map.MapContent
            content.layers.add(
              new org.geotools.map.FeatureLayer(layer.source, style.underlying)
            )
            renderer.setMapContent(content)
            renderer.paint(graphics, window, bounds)
            content.dispose()
          }
      }
  }

  object Viewport {
    /**
     * From a list of layers and a desired screen size, determine the size of
     * viewing area that preserves the aspect ratio of the layers' bounds and
     * fits within the given size.
     */
    def frame(
      envelope: Bounds,
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
    def pad(envelope: Bounds, window: (Int, Int) = (500, 500))
    : Bounds = {
      val aspect = envelope.height / envelope.width
      val idealAspect = window._2.toDouble / window._1
      val padded = (
        if (aspect < idealAspect) {
          val height = envelope.height * (idealAspect/aspect)
          Bounds(
            envelope.minX, envelope.centre.y - height/2d,
            envelope.maxX, envelope.centre.y + height/2d
          )
        } else {
          val width = envelope.width * (aspect/idealAspect)
          Bounds(
            envelope.centre.x - width/2d, envelope.minY,
            envelope.centre.x + width/2d, envelope.maxY
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
  def PNG[T](sink: Sink[T], window: (Int, Int) = (500, 500)): Context[T] =
    new Context[T] { 
      def apply(draw: (Graphics2D, Rectangle) => Unit): T = {
        import java.awt.image.BufferedImage
        val (width, height) = window
        val image = new BufferedImage(
          width, height,
          BufferedImage.TYPE_INT_ARGB
        )
        val graphics = image.createGraphics()
        draw(graphics, new Rectangle(0, 0, width, height))
        graphics.dispose()
        sink { out => javax.imageio.ImageIO.write(image, "PNG", out) }
      }
    }

  def JPEG[T](sink: Sink[T], window: Rectangle = new Rectangle(0, 0, 500, 500)): Context[T] =
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

  def GIF[T](sink: Sink[T], window: Rectangle = new Rectangle(0, 0, 500, 500)): Context[T] =
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
        sink { javax.imageio.ImageIO.write(image, "GIF", _) }
      }
    }

  def PDF[T](sink: Sink[T],
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

          draw(graphic, paintArea)
          graphic.dispose()
          content.addTemplate(template, 0, 0)
          document.close()
        }
      }
    }


  def render(bounds: Bounds, layers: Seq[SpatialRenderable]): Renderable =
    Renderable.chain(layers.map(_ apply bounds))
}
