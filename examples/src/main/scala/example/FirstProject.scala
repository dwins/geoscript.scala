package org.geoscript.example

import org.geoscript._

object FirstProject extends App {
  val shp = layer.Shapefile(args(0))
  val length = shp.withAll { features =>
    features.map(_.geometry.length).sum
  }

  println("Total Length %f".format(length));
}
