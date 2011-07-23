name := "geoscript"

organization := "org.geoscript"

version := "0.7.1"

resolvers ++= Seq(
  "OSGeo" at "http://download.osgeo.org/webdav/geotools/",
  "OpenGeo" at "http://repo.opengeo.org/"
)

libraryDependencies ++= {
  val gtVersion = "2.7.0.1"
  Seq(
    "org.geotools" % "gt-main" % gtVersion,
    "org.geotools" % "gt-epsg-hsql" % gtVersion,
    "org.geotools" % "gt-shapefile" % gtVersion,
    "org.geotools" % "gt-render" % gtVersion,
    "org.geotools.jdbc" % "gt-jdbc-postgis" % gtVersion,
    "org.geotools.jdbc" % "gt-jdbc-spatialite" % gtVersion,
    "net.sf.json-lib" % "json-lib" % "2.3" classifier "jdk15",
    "javax.media" % "jai_core" % "1.1.3",
    "org.scala-tools.testing" %% "specs" % "1.6.7.2" % "test",
    "xml-apis" % "xml-apis-xerces" % "2.7.1" from "http://repo.opengeo.org/xml-apis/xml-apis-xerces/2.7.1/xml-apis-xerces-2.7.1.jar"
  )
}

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

libraryDependencies <+= (organization, version) apply { _ %% "geocss" % _ }
