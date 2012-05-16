package org.geoscript.example

import org.geoscript._
import feature.{ Field, GeoField, Schema }

object Shp2Shp extends App {
  val Array(sourcefile, destname, proj) = args take 3
  val source = layer.Shapefile(sourcefile)
  val destSchema = Schema(destname,
    source.schema.fields map {
      case (g: GeoField) => g.withProjection(proj)
      case (f: Field) => f
    }
  )
  val dest = source.workspace.create(destSchema)
  source.withAll { fs =>
    dest ++= fs.toIterable
  }
}
