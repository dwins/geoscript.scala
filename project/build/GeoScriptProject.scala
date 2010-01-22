import sbt._

class GeoScriptProject(info: ProjectInfo) extends ParentProject(info) {
  lazy val library = 
    project("geoscript", "GeoScript Library", new GeoScriptLibrary(_))
  lazy val examples = project("examples", "GeoScript Examples", library)

  lazy val console = task {
    library.act("console")
  } describedAs "Alias for library's console task."

  lazy val run = task { (args: Array[String]) => task { 
    examples.call("run", args) 
  } } describedAs "Alias for examples' run task."

  class GeoScriptLibrary(info: ProjectInfo) extends DefaultProject(info) {
    val osgeo = "OSGeo Maven Repository" at 
        "http://download.osgeo.org/webdav/geotools/"
    val opengeo = "OpenGeo Maven Repository" at 
        "http://repo.opengeo.org/"

    val java_net = "Java.net Maven Repository" at 
        "http://download.java.net/maven/2/"

    val gtVersion = "[2.6.1,2.7.0["
        
    val gtMain = "org.geotools" % "gt-main" % gtVersion
    val gtReferencing = "org.geotools" % "gt-epsg-hsql" % gtVersion
    val gtShapefile = "org.geotools" % "gt-shapefile" % gtVersion
    val gtJDBC = "org.geotools" % "gt-jdbc" % gtVersion
    val gtDirectory = "org.geotools" % "gt-directory" % gtVersion 
    val gtPostgis = "org.geotools.jdbc"   % "gt-jdbc-postgis" % gtVersion 
    val gtSpatiaLite = "org.geotools.jdbc" % "gt-jdbc-spatialite" % gtVersion

    val jai = "javax.media" % "jai_core" % "1.1.3"

    val specs = "org.specs" % "specs" % "[1.4.0,1.5[" % "test"
  }
}
