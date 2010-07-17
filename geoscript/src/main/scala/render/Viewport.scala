package org.geoscript

import collection.JavaConversions._
import com.vividsolutions.jts.{geom=>jts}
import java.awt.RenderingHints
import RenderingHints._

package render {
  case class Viewport(bounds: jts.Envelope) {
    def render(layers: Seq[(layer.Layer, style.Style)]) = {
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

      val image = new java.awt.image.BufferedImage(
        512, 512, java.awt.image.BufferedImage.TYPE_INT_ARGB
      )
      val graphics = image.createGraphics()
      renderer.paint(
        graphics,
        new java.awt.Rectangle(0, 0, 512, 512),
        bounds
      )
      graphics.dispose()
      val output = new java.io.FileOutputStream("result.png")
      javax.imageio.ImageIO.write(image, "PNG", output)
      output.flush()
      output.close()
    }
  }
}
