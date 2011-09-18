package org.geoscript.example

import org.geoscript._
import feature._
import layer._
import projection._

object Shp2Shp extends App with GeoScript {
    val List(sourcefile, destname, proj) = args.toList take 3
    val source = Shapefile(sourcefile)
    val destSchema = Schema(destname,
      source.schema.fields map {
        case g: GeoField => Field(g.name, g.binding, proj)
        case f: Field => f
      }
    )
    val dest = source.workspace.create(destSchema)
    dest ++= source.features map { f =>
      f.update(destSchema.geometry.name -> (f.geometry in proj))
    }
}
