package org.geoscript

import collection.JavaConversions._
import com.vividsolutions.jts.{geom=>jts}
import java.awt.RenderingHints

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
          new org.geotools.map.DefaultMapLayer(data.source, style.unwrapped)
        )
      }
      renderer.setContext(context)
      renderer.paint(graphics, window, bounds)
    }

    def render(layers: Seq[(layer.Layer, style.Style)]): RichImage =
      render(layers, Viewport.frame(layers))

    def render(
      layers: Seq[(layer.Layer, style.Style)],
      window: java.awt.Rectangle
    ): RichImage = {
      import java.awt.image.BufferedImage
      val image = new BufferedImage(
        window.width,
        window.height,
        BufferedImage.TYPE_INT_ARGB
      )
      val graphics = image.createGraphics()
      draw(graphics, layers, window)
      graphics.dispose()
      image
    }
  }

  object Viewport {
    /**
     * From a list of layers and a desired screen size, determine the size of
     * viewing area that preserves the aspect ratio of the layers' bounds and
     * fits within the given size.
     */
    def frame(
      layers: Seq[(layer.Layer, style.Style)],
      maximal: (Int, Int) = (500, 500)
    ): java.awt.Rectangle = {
      val realWorldBounds = new jts.Envelope()
      layers.foreach { l => realWorldBounds.expandToInclude(l._1.bounds) }
      val aspect = realWorldBounds.getHeight() / realWorldBounds.getWidth()
      val idealAspect = maximal._2.toDouble / maximal._1.toDouble
      if (aspect < idealAspect) {
        new java.awt.Rectangle(0, 0, maximal._1, (maximal._2 * aspect).toInt)
      } else {
        new java.awt.Rectangle(0, 0, (maximal._1 / aspect).toInt, maximal._2)
      }
    }

    // /** 
    //  * From a real-world envelope and a Rectangle representing the display
    //  * area, expand the envelope so that it matches the aspect ratio of the
    //  * desired viewing window.  The center of the envelope is preserved.
    //  */
    // def pad(envelope: jts.Envelope, window: java.awt.Rectangle)
    // : jts.Envelope = {
    //   val aspect = envelope.getHeight() / 
    // }
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
