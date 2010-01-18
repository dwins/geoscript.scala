package org.geoscript.example

import org.geoscript._

object Identify extends GeoScript with org.geoscript.feature.GeoCrunch {
  def main(args: Array[String]) {
    val shp = layer.Shapefile(args(0))
    println("Schema for %s".format(shp.schema.name))
    for (field <- shp.schema.fields) {
      println(field.name, field.binding.getSimpleName)
    }
  }
}
