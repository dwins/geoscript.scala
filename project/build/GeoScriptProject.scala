import sbt._

class GeoScriptProject(info: ProjectInfo) extends ParentProject(info) {
  // some common dependency configuration
  val gtVersion = "2.7.0.1"
  override def repositories = super.repositories ++ Set(
    "OSGeo" at "http://download.osgeo.org/webdav/geotools/",
    "OpenGeo" at "http://repo.opengeo.org/",
    "Java.net" at "http://download.java.net/maven/2/",
    "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  )

  // subproject declarations
  lazy val docs = project("docs", "docs", new SphinxProject(_))
  lazy val examples = project("examples", "examples", library)
  lazy val geocss = project("geocss", "geocss", new GeoCSS(_))
  lazy val library = 
    project("geoscript", "library", new GeoScriptLibrary(_), geocss)

  // delegate to examples for a couple of common tasks
  lazy val console = task {
    library.act("console")
  } describedAs "Alias for library's console task."

  lazy val run = task { (args: Array[String]) => task { 
    examples.call("run", args) 
  } } describedAs "Alias for examples' run task."

  // subproject definitions
  class GeoScriptLibrary(info: ProjectInfo) extends DefaultProject(info) {
    override def libraryDependencies = super.libraryDependencies ++ Set(
      "org.geotools" % "gt-main" % gtVersion,
      "org.geotools" % "gt-epsg-hsql" % gtVersion,
      "org.geotools" % "gt-shapefile" % gtVersion,
      "org.geotools" % "gt-jdbc" % gtVersion,
      "org.geotools" % "gt-render" % gtVersion,
      "org.geotools.jdbc" % "gt-jdbc-postgis" % gtVersion,
      "org.geotools.jdbc" % "gt-jdbc-spatialite" % gtVersion,
      "org.scala-lang" % "scala-swing" % "2.8.0",
      "net.sf.json-lib" % "json-lib" % "2.3" classifier "jdk15",
      "javax.media" % "jai_core" % "1.1.3",
      "org.scala-tools.testing" %% "specs" % "1.6.7.2" % "test"
    )
  }

  class GeoCSS(info: ProjectInfo) extends DefaultProject(info) {
    override def managedStyle = ManagedStyle.Maven
    lazy val publishTo = "DAV" at "http://repo.opengeo.org/"
    Credentials(Path.userHome / ".ivy2" / ".credentials", log)

    override def libraryDependencies = super.libraryDependencies ++ Set(
      "junit" % "junit" % "4.2" % "test",
      "org.scala-tools.testing" %% "specs" % "1.6.7.2" % "test",
      "org.geotools" % "gt-main" % gtVersion,
      "org.geotools" % "gt-cql" % gtVersion,
      "org.geotools" % "gt-epsg-hsql" % gtVersion,
      "org.geotools" % "gt-jdbc" % gtVersion,
      "org.geotools" % "gt-shapefile" % gtVersion,
      "xml-apis" % "xml-apis-xerces" % "2.7.1" from "http://repo.opengeo.org/xml-apis/xml-apis-xerces/2.7.1/xml-apis-xerces-2.7.1.jar"
    )
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
