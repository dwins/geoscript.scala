package org.geoscript.example

import org.geoscript._

object AllValid extends App {
  val shp = layer.Shapefile(args.head)

  val invalid = shp.features filter { f => !f.geometry.isValid } toSeq
  
  println("Found %s invalid features.".format(invalid.size))
  for (f <- invalid) println(f.id)
}
