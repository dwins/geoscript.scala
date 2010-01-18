package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import layer._

object FirstProject extends GeoScript with feature.GeoCrunch {
  def main(args: Array[String]) {
    val file = promptShapeFile
    val shp = Shapefile(file.toString)

    val length = shp.features.foldLeft(0d) {
      (l, f) => l + f.geometry.length
    }

    println("Total Length %f".format(length));
  }
}
