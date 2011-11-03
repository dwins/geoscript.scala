package org.geoscript.example

import org.geoscript._

object Identify extends App {
  val shp = layer.Shapefile(args(0))
  println("Schema for %s".format(shp.schema.name))
  for (field <- shp.schema.fields) println(field)
}
