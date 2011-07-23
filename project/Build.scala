import sbt._

object MyBuild extends Build {
  lazy val root = Project("root", file(".")) aggregate(library, css)
  lazy val css = Project("css", file("geocss"))
  lazy val examples = Project("examples", file("examples")) dependsOn(library)
  lazy val library = Project("library", file("geoscript")) dependsOn(css)
}
