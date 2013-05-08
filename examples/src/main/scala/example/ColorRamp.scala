package org.geoscript.example

import org.geoscript.feature._
import org.geoscript.layer._
import org.geoscript.style._

object ColorRamp extends org.geoscript.feature.GeoCrunch {
  // def main(args: Array[String]) = {
  //   val Array(shapefile, property, sldfile) = args take 3

  //   val shp = Shapefile(shapefile)
  //   val style = colorRamp(shp, property)

  //   val xformer = new org.geotools.styling.SLDTransformer
  //   val sldStream = new java.io.FileOutputStream(sldfile)
  //   xformer.setIndentation(2)
  //   xformer.transform(style, sldStream)
  //   sldStream.flush()
  //   sldStream.close()
  // }

  // def hex(c: java.awt.Color): Paint = 
  //   Color(literal(
  //     "#%02x02x02x".format(c.getRed, c.getGreen, c.getBlue)))

  // def colorRamp(data: Layer, propertyName: String): Style = {
  //   val propertyView = data.features.view.map(f => f.get[Double](propertyName))
  //   val min = propertyView.min
  //   val max = propertyView.max

  //   val k = 10
  //   val breaks = for (i <- (0 to k)) yield (i * max + (k - i) * min) / k
  //   val ranges = (breaks sliding 2).toSeq
  //   val colors = (Seq.iterate(java.awt.Color.RED, k){ _.darker }).reverse
  //   val rules = 
  //     for {
  //       (Seq(min, max), color) <- ranges zip colors
  //       filter = "%s BETWEEN %f AND %f".format(propertyName, min, max)
  //     } yield
  //       Fill(hex(color)) where cql(filter)
  //   rules.reduce(_ and _).build
  // }
  ???
}
