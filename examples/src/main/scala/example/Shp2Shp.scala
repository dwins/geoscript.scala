package org.geoscript.example

import org.geoscript.feature._
import org.geoscript.feature.schemaBuilder._
import org.geoscript.layer._
import org.geoscript.projection._ 
import org.geoscript.workspace._
/// import feature.{ Field, GeoField, Schema }

/**
 * An example app for copying a shapefile while transforming the data to a new
 * coordinate reference system.
 */
object Shp2Shp extends App {
  // by extending the scala.App trait from the Scala standard library, we avoid
  // writing the def main(args: Array[String]) that is otherwise required for 
  // an object to be executable.

  if (args.size < 3) {
    // The variable `args` provided by scala.App is a Seq[String] containing the
    // command line arguments.  We need at least three: the source path, new
    // path, and new coordinate reference system.
    println(
""" |Usage: Shp2Shp <source shapefile> <destination shapefile> <srid>
    |An example script demonstrating the use of GeoScript to copy a Shapefile
    |while reprojecting the data.
    |""".stripMargin)
    System.exit(0) 
  }

  // for convenience, we create a little function for connecting to shapefiles.
  val connect = (path: String) =>
    new org.geotools.data.shapefile.ShapefileDataStore(
      new java.io.File(path).toURI.toURL): Workspace

  // Here we use a pattern match to concisely extract the arguments into
  // individual variables.
  val Array(sourcePath, destinationPath, srid) = (args take 3)
  val source = connect(sourcePath)
  val sourceLayer = source.layers.head
  val destination = connect(destinationPath)

  // Now we try to lookup the SRID in GeoTools' EPSG database.
  // This might correctly find nothing (as opposed to failing due to hardware or
  // software error) so lookupEPSG returns an Option[Projection] rather than
  // just a projection.  We can handle the possible absence using a match
  // statement.
  lookupEPSG(srid) match {
    case None => // No projection was found
      println(s"Not a known reference system: $srid")
    case Some(proj) => // Inside this "case" statement, the projection is in the variable "proj"
      val dstSchema = reproject(sourceLayer.schema, proj)
      destination.createSchema(dstSchema)
      val dstLayer = destination.layers.head
      // todo: explain how to use Either and for-comprehensions to avoid the nested matches here.
      dstLayer.writable match {
        case None => 
          println("Destination layer not writable!")
        case Some(writable) =>
          writable ++= (sourceLayer.features.map(reproject(_, proj)))
      }
  }
}
