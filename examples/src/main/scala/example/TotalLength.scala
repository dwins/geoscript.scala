package org.geoscript.example

import org.geoscript.feature._
import org.geoscript.geometry._
import org.geoscript.layer._
import org.geoscript.workspace._

/**
 * An example app for printing out the total length of all geometries in a
 * shapefile.  Inspired by the "FirstProject" from the GeoTools documentation: 
 */
object TotalLength extends App {
  // by extending the scala.App trait from the Scala standard library, we avoid
  // writing the def main(args: Array[String]) that is otherwise required for 
  // an object to be executable.

  if (args.isEmpty) {
    // The variable `args` provided by scala.App is a Seq[String] containing the
    // command line arguments.  We need at least one so we can interpret it as a
    // path!
    println(
""" |Usage: TotalLength <path>
    |An example script demonstrating the use of GeoScript to compute the total
    |length of geometries in a Shapefile and print it.
    |""".stripMargin)
    System.exit(0) 
  }

  // we take the "head" (first element) of the command line parameters to use as
  // the path to the Shapefile we will be inspecting.
  val path: String = args.head

  // Since the ShapefileDatastore constructor requires a URL, we need to perform
  // some gymnastics.  By constructing a java.io.File, we can handle relative
  // paths.  Converting to a URI before converting to URL is recomended because
  // of some problems with file path handling in the Java standard library.
  // GeoScript doesn't attempt to address this issue, but the Scala-IO and
  // Rapture.IO projects are two options that you could investigate for more
  // convenient path handling.
  val url: java.net.URL = new java.io.File(path).toURI.toURL

  // Now we construct a Workspace for the Shapefile.  Consult the GeoTools
  // documentation for other types of DataStore that may be used.
  val workspace: Workspace = new org.geotools.data.shapefile.ShapefileDataStore(url)

  // Because we know that a Shapefile contains exactly one layer, we can simply
  // use the first and only layer from the workspace.
  val layer: Layer = workspace.layers.head

  // While layer.features is a special FeatureCollection object with
  // GIS-specific methods and operations, it is also integrated with the Scala
  // collections framework.  This makes operations like extracting a field and
  // summing it quite straightforward; see
  // http://docs.scala-lang.org/overviews/collections/introduction.html
  val totalLength: Double = layer.features.map(_.geometry.length).sum

  println(s"Total geometry length: $totalLength")
}
