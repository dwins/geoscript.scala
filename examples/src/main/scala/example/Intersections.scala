package org.geoscript.example

import org.geoscript._

object Intersections {
  def process(src: layer.Layer, dest: layer.Layer, joinField: String) {
    println("Processing %s".format(src.schema.name))

    src.withAll { features =>
      for (feat <- features) {
        src.withFiltered(filter.Filter.intersects(feat.geometry)) {
          intersections =>

          dest ++= 
            intersections.filter(_.id > feat.id).map { corner =>
              feature.Feature(
                "geom" -> (feat.geometry intersection corner.geometry),
                (joinField + "Left") -> feat.get[Any](joinField),
                (joinField + "Right") -> corner.get[Any](joinField)
              )
            }
        }
      }
    }

    println("Found %d intersections".format(dest.count))
  }

  import feature.{ Field, Schema, bind }
  def rewrite(schema: Schema, fieldName: String): Schema = 
    Schema(
      schema.name + "_intersections",
      Seq(
        bind[geometry.Geometry]("geom",
          schema.geometry.getCoordinateReferenceSystem),
        bind[String](fieldName + "Left"),
        bind[String](fieldName + "Right")))

  def main(args: Array[String]) = {
    if (args.length == 0) {
      println("You need to provide the path to a shapefile as an argument to this example.")
    } else {
      val src = layer.Shapefile(args(0))
      val joinField = 
        src.schema.fields.find { _.binding == classOf[String] } match {
          case Some(f) => f.name
          case None => "id"
        }
      val dest = src.workspace.create(rewrite(src.schema, joinField))
      process(src, dest, joinField)
    }
  }
}
