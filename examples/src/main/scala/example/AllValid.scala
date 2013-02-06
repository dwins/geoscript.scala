package org.geoscript.example

import org.geoscript.layer._
import org.geoscript.feature._

object AllValid extends App {
  val shp = Shapefile(args.head)

  val invalid = shp.features.filterNot(_.geometry.isValid).toSeq
  
  println("Found %s invalid features.".format(invalid.size))
  for (f <- invalid) println(f.id)
}
