import sbt._

class GeoServerCSSProject(info: ProjectInfo) extends DefaultProject(info) {
  val gtVersion="2.6-SNAPSHOT"

  override def managedStyle = ManagedStyle.Maven
  lazy val publishTo =
    Resolver.file("maven-local", Path.userHome / ".m2" / "repository" asFile)

  override def repositories = super.repositories ++ Set(
    "OpenGeo Maven Repository" at "http://repo.opengeo.org/"
  )

  override def libraryDependencies = super.libraryDependencies ++ Set(
    "org.scalatest" % "scalatest" % "1.0" % "test",
    "junit" % "junit" % "4.2" % "test",
    "org.geotools" % "gt-main" % gtVersion,
    "org.geotools" % "gt-cql" % gtVersion,
    "org.geotools" % "gt-epsg-hsql" % gtVersion,
    "org.geotools" % "gt-jdbc" % gtVersion,
    "org.geotools" % "gt-shapefile" % gtVersion,
    "xml-apis" % "xml-apis-xerces" % "2.7.1" from "http://repo.opengeo.org/xml-apis/xml-apis-xerces/2.7.1/xml-apis-xerces-2.7.1.jar"
  )
}
