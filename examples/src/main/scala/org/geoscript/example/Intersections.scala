package org.geoscript.example

import org.geoscript._

object Intersections extends GeoScript with feature.GeoCrunch {
  def process(src: layer.Layer, dest: layer.Layer, joinField: String) {
    println("Processing %s".format(src.schema.name))

    for (feat <- src.features) {
      val intersections = 
        src.filter(filter.Intersects(feat.geometry))
      dest ++= 
        intersections.filter(_.id > feat.id).map { corner =>
          layer.Feature(
            "geom" -> (feat.geometry intersection corner.geometry),
            (joinField + "Left") -> feat.get(joinField),
            (joinField + "Right") -> corner.get(joinField)
          )
        }
    }

    println("Found %d intersections".format(dest.count))
  }

  def rewrite(schema: layer.Schema, fieldName: String): layer.Schema = 
    layer.Schema(
      schema.name + "_intersections",
      layer.Field("geom", classOf[com.vividsolutions.jts.geom.Geometry]),
      layer.Field(fieldName + "Left", classOf[String]),
      layer.Field(fieldName + "Right", classOf[String])
    )

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
