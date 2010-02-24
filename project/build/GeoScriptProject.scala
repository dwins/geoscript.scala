import sbt._

class GeoScriptProject(info: ProjectInfo) extends ParentProject(info) {
  lazy val library = 
    project("geoscript", "GeoScript Library", new GeoScriptLibrary(_))
  lazy val docs =
    project("docs", "GeoScript Documentation", new SphinxProject(_))
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

    val gtVersion = "2.6.1"
        
    val gtMain = "org.geotools" % "gt-main" % gtVersion
    val gtReferencing = "org.geotools" % "gt-epsg-hsql" % gtVersion
    val gtShapefile = "org.geotools" % "gt-shapefile" % gtVersion
    val gtJDBC = "org.geotools" % "gt-jdbc" % gtVersion
    val gtDirectory = "org.geotools" % "gt-directory" % gtVersion 
    val gtPostgis = "org.geotools.jdbc"   % "gt-jdbc-postgis" % gtVersion 
    val gtSpatiaLite = "org.geotools.jdbc" % "gt-jdbc-spatialite" % gtVersion

    val scalaSwing = "org.scala-lang" % "scala-swing" % ("2.7.7")
    val jai = "javax.media" % "jai_core" % "1.1.3"

    val specs = "org.specs" % "specs" % "[1.4.0,1.5[" % "test"
  }

  class SphinxProject(val info: ProjectInfo) 
  extends ReflectiveTasks with ReflectiveMethods {
    import Process._
    val doc = task { 
      try {
        new java.lang.ProcessBuilder(
          "sphinx-build",
          "-b", "html",
          "-d", "target/doctrees",
          ".",
          "target/html"
        ) directory new java.io.File("docs")! match {
          case 0 => None
          case error => Some("Sphinx failed with error code %d".format(error))
        }
      } catch {
        case ex => Some("Couldn't run Sphinx due to %s".format(ex.getMessage))
      }
    }

    val clean = task {
      FileUtilities.clean(outputDirectories, log)
      None
    }

    override val dependencies = Nil
  }
}
