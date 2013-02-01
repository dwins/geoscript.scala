package org.geoscript.example

import org.geoscript.layer._
import org.geoscript.geometry._

object FirstProject extends App {
  val shp = Shapefile(args(0))
  val length = shp.features.map(_.geometry.length).sum

  println("Total Length %f".format(length));
}
