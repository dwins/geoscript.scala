package org.geoscript.example

import org.geoscript.feature._, schemaBuilder._
import org.geoscript.layer._
import org.geoscript.projection._ 
/// import feature.{ Field, GeoField, Schema }

object Shp2Shp extends App {
  val Array(sourcefile, destname, projTxt) = args take 3
  val source = Shapefile(sourcefile)
  val proj = lookupEPSG(projTxt).get
  source.workspace.addLayer(reproject(source, proj))
}
