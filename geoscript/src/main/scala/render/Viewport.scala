package org.geoscript

import collection.JavaConversions._
import com.vividsolutions.jts.{geom=>jts}
import java.awt.RenderingHints
import RenderingHints._

package render {
  class RichImage(private val im: java.awt.image.RenderedImage) {
    def writeTo(formatter: java.awt.image.RenderedImage => Unit) = formatter(im)
  }

  object RichImage {
    implicit def wrap(im: java.awt.image.RenderedImage) = new RichImage(im)
    implicit def unwrap(ri: RichImage): java.awt.image.RenderedImage = ri.im
  }

  trait Sink { def out: java.io.OutputStream }

  object Sink {
    implicit def fromFile(f: java.io.File) =
      new Sink { def out = new java.io.FileOutputStream(f) }

    implicit def fromPath(path: String) = 
      fromFile(new java.io.File(".", path))

    implicit def fromStream(o: java.io.OutputStream) = 
      new Sink { val out = o } 
  }

  case class Viewport(bounds: jts.Envelope) {
    def draw(
      graphics: java.awt.Graphics2D,
      layers: Seq[(layer.Layer, style.Style)],
      window: java.awt.Rectangle
    ) = {
      val renderer = new org.geotools.renderer.lite.StreamingRenderer()
      renderer.setJava2DHints(new RenderingHints(Map(
        KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON,
        KEY_TEXT_ANTIALIASING -> VALUE_TEXT_ANTIALIAS_ON
      )))
      val context = new org.geotools.map.DefaultMapContext()
      for ((data, style) <- layers) {
        context.addLayer(
          new org.geotools.map.DefaultMapLayer(data.source, style.unwrapped)
        )
      }
      renderer.setContext(context)
      renderer.paint(graphics, window, bounds)
    }

    def render(layers: Seq[(layer.Layer, style.Style)]): RichImage = {
      import java.awt.image.BufferedImage
      val image = new BufferedImage(512, 512, BufferedImage.TYPE_INT_ARGB)
      val graphics = image.createGraphics()
      draw(graphics, layers, new java.awt.Rectangle(0, 0, 512, 512))
      graphics.dispose()
      image
    }
  }
}

package object render {
  def GIF (sink: Sink)(im: java.awt.image.RenderedImage) { 
    val out = sink.out
    javax.imageio.ImageIO.write(im, "GIF", out)
    out.close()
  }

  def JPEG (sink: Sink)(im: java.awt.image.RenderedImage) { 
    val out = sink.out
    javax.imageio.ImageIO.write(im, "JPEG", out)
    out.close()
  }

  def PNG (sink: Sink)(im: java.awt.image.RenderedImage) { 
    val out = sink.out
    javax.imageio.ImageIO.write(im, "PNG", out)
    out.close()
  }
}
