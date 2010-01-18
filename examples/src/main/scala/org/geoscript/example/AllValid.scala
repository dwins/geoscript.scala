package org.geoscript.example

import org.geoscript._

object AllValid extends GeoScript with feature.GeoCrunch {
  def main(args: Array[String]) = {
    val file = promptShapeFile.toURI.toString
    val shp = layer.Shapefile(file)
  
    val invalid = shp.features filter { f => !f.geometry.isValid } toSeq
    
    println("Found %s invalid features.".format(invalid.size))
    for (f <- invalid) println(f.id)
  }
}
