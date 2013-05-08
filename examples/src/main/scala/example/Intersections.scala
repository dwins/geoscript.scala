package org.geoscript.example

import org.geoscript.feature._
import org.geoscript.filter._
import org.geoscript.filter.builder._
import org.geoscript.geometry._
import org.geoscript.layer._
import org.geoscript.workspace._

/**
 * An example app for creating a shapefile containing all intersections between
 * two input Shapefiles
 */
object Intersections extends App {
  // by extending the scala.App trait from the Scala standard library, we avoid
  // writing the def main(args: Array[String]) that is otherwise required for 
  // an object to be executable.

  if (args.size < 3) {
    // We need three arguments: first shapefile to scan, second shapefile to
    // scan, shapefile to store results.
    println(
""" |Usage: Intersections <first shapefile> <second shapefile> <output shapefile>
    |Computes all intersections between the two input shapefiles and stores the
    |results in the output shapefile.  The output will also have two fields
    |named left_id and right_id containing the ids of the features that
    |intersected. (This is just an example - NOTE that shapefile features do not
    |have stable identifiers.)""".stripMargin)
    System.exit(0)
  }

  // for convenience, we create a little function for connecting to shapefiles.
  val connect = (path: String) =>
    new org.geotools.data.shapefile.ShapefileDataStore(
      new java.io.File(path).toURI.toURL): Workspace

  // Here we use a pattern match to concisely extract the arguments into
  // individual variables.
  val Array(leftPath, rightPath, outputPath) = (args take 3)
  val leftLayer = connect(leftPath).layers.head
  val rightLayer = connect(rightPath).layers.head
  
  // There are a few different ways to compute the intersections.
  // The simplest is to use a Scala for-comprehension.
  // val intersections =
  //   for {
  //     l <- leftLayer.features
  //     r <- rightLayer.features
  //     if l.geometry intersects r.geometry
  //   } yield (l.geometry intersection r.geometry, l.id, r.id)

  // This produces correct results, but there are some performance problems.
  // * It fetches all features from the 'right' layer on each step of iterating
  //   through the 'left' layer.  This might mean a lot of disk access!
  // * The results are stored in memory.  Since we're just going to write the
  //   features to a new shapefile it would be nice to avoid that.  It would
  //   save some memory, and also might complete faster if we can start writing
  //   the results before we finish finding all the intersections.
  //
  // We can avoid repetitive requests to the underlying store by copying all the
  // features into an in-memory collection before scanning.

  // val leftFeatures = leftLayer.features.to[Vector]
  // val rightFeatures = rightLayer.features.to[Vector]

  // val intersections2 = 
  //   for {
  //     l <- leftFeatures
  //     r <- rightFeatures
  //     if l.geometry intersects r.geometry
  //   } yield (l.geometry intersection r.geometry, l.id, r.id)

  // This trades off memory in order to speed up the processing, so it's
  // still only going to work for small datasets.  Instead of performing the
  // filtering in Scala code, we can use the GeoTools Query system to have it
  // performed by the datastore itself.  Depending on the datastore filters will
  // be more or less completely executed by the underlying engine.  For example, 
  // filters executed against a Postgis database can be largely converted to
  // SQL.  For Shapefiles most filter operations are executed in-process, but
  // they are at least able to take advantage of a spatial index.

  val intersections3 =
    for {
      l <- leftLayer.features
      r <- rightLayer.filter(
        Literal(l.geometry) intersects Property(rightLayer.schema.geometryField.name))
    } yield (l.geometry intersection r.geometry, l.id, r.id)

  intersections3.foreach(println)

  // require(intersections.toSeq == intersections.toSeq.distinct)

  // def process(src: layer.Layer, dest: layer.Layer, joinField: String) {
  //   println("Processing %s".format(src.schema.name))

  //   for (feat <- src.features) {
  //     val intersections = 
  //       src.filter(filter.Filter.intersects(feat.geometry))
  //     dest ++= 
  //       intersections.filter(_.id > feat.id).map { corner =>
  //         feature.Feature(
  //           "geom" -> (feat.geometry intersection corner.geometry),
  //           (joinField + "Left") -> feat.get[Any](joinField),
  //           (joinField + "Right") -> corner.get[Any](joinField)
  //         )
  //       }
  //   }

  //   println("Found %d intersections".format(dest.count))
  // }

  // def rewrite(schema: feature.Schema, fieldName: String): feature.Schema = 
  //   feature.Schema(
  //     schema.name + "_intersections",
  //     feature.Field(
  //       "geom",
  //       classOf[com.vividsolutions.jts.geom.Geometry],
  //       schema.geometry.projection
  //     ),
  //     feature.Field(fieldName + "Left", classOf[String]),
  //     feature.Field(fieldName + "Right", classOf[String])
  //   )

  // def main(args: Array[String]) = {
  //   if (args.length == 0) {
  //     println("You need to provide the path to a shapefile as an argument to this example.")
  //   } else {
  //     val src = layer.Shapefile(args(0))
  //     val joinField = 
  //       src.schema.fields.find { _.gtBinding == classOf[String] } match {
  //         case Some(f) => f.name
  //         case None => "id"
  //       }
  //     val dest = src.workspace.create(rewrite(src.schema, joinField))
  //     process(src, dest, joinField)
  //   }
  // }
}
