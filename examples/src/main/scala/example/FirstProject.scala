package org.geoscript.example

import org.geoscript._

object FirstProject extends App {
  val shp = layer.Shapefile(args(0))
  println("Total Length %f".format(shp.size));
}
