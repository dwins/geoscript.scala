import sbt._
import Keys._

object MyBuild extends Build {
  lazy val root = Project("root", file(".")) aggregate(css, docs, library)
  lazy val css = Project("css", file("geocss"))
  lazy val examples = Project("examples", file("examples")) dependsOn(library)
  lazy val library = Project("library", file("geoscript")) dependsOn(css)
  lazy val docs = Project(
    "docs", file("docs"),
    settings = Defaults.defaultSettings ++ Seq(
      docDirectory <<= target(_ / "doc"),
      sphinxDir <<= docDirectory(_ / "sphinx"),
      sphinxSource <<= baseDirectory.identity,
      sphinxBuild := "sphinx-build",
      sphinxOpts := Nil,
      sphinx <<= (sphinxBuild, sphinxSource, sphinxDir, sphinxOpts) map (runSphinx),
      watchSources <<= (baseDirectory, target) map { (b, t) => (b ** "*") --- (t ** "*") get }
    )
  )

  lazy val sphinx = 
    TaskKey[java.io.File]("sphinx", "runs sphinx documentation generator")
  lazy val sphinxBuild = 
    SettingKey[String]("sphinx-build", "command to use when building sphinx")
  lazy val sphinxOpts =
    SettingKey[Seq[String]]("sphinx-opts", "options to pass to sphinx-build script")
  lazy val sphinxSource =
    SettingKey[java.io.File]("sphinx-source", "source directory for sphinx docs")
  lazy val sphinxDir =
    SettingKey[java.io.File]("sphinx-dir", "output directory for sphinx docs")

  def runSphinx(script: String, input: java.io.File, output: java.io.File, opts: Seq[String]) = {
    val cmd = Seq(script) ++ opts ++ Seq("-b", "html", "-d", 
      (output / "doctrees").getAbsolutePath,
      input.getAbsolutePath,
      (output / "html").getAbsolutePath)
    cmd ! ;
    output / "html"
  }
}
