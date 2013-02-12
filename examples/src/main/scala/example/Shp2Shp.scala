package org.geoscript.example

import org.geoscript._
import feature.{ Field, GeoField, Schema }

object Shp2Shp extends App {
  val Array(sourcefile, destname, proj) = args take 3
  val source = layer.Shapefile(sourcefile)
  val destSchema = Schema(destname,
    source.schema.fields map {
      case (g: GeoField) => g.copy(projection = projection.lookupEPSG(proj).get)
      case (f: Field) => f
    }
  )
  val dest = source.workspace.create(destSchema)
  dest ++= source.features map { f =>
    f.update(destSchema.geometry.name -> (f.geometry /* in proj */))
  }
}
