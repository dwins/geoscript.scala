import sbt._, Keys._, Defaults.defaultSettings

object GeoScript extends Build {
  lazy val gtVersion = 
    SettingKey[String]("gt-version", "Version number for GeoTools modules")

  val meta =
    Seq[Setting[_]](
      organization := "org.geoscript",
      version := "0.8.2",
      gtVersion := "9.3",
      scalaVersion := "2.10.0", 
      scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint", "-unchecked"),
      javacOptions ++= Seq("-source", "6"),
      publishTo := Some(Resolver.file("file", file("release")))
    )

  val common = 
    Seq[Setting[_]](
      fork := true,
      resolvers ++= Seq(
        "opengeo" at "http://repo.opengeo.org/",
        "osgeo" at "http://download.osgeo.org/webdav/geotools/"
      )
    ) ++ meta ++ defaultSettings

  val sphinxSettings =
    Seq(
      baseDirectory <<= thisProject(_.base),
      target <<= baseDirectory / "target",
      sphinxDir <<= crossTarget(_ / "sphinx"),
      sphinxSource <<= baseDirectory(_ / "src" / "main" / "sphinx"),
      sphinxBuild := "sphinx-build",
      sphinxOpts := Nil,
      sphinx <<= (sphinxBuild, sphinxSource, sphinxDir, sphinxOpts) map (runSphinx),
      watchSources <<= (baseDirectory, target) map { (b, t) => (b ** "*") --- (t ** "*") get }
    )

  lazy val root =
    Project("root", file("."), settings = common ++ Seq(fork in test := false, publish := false)) aggregate(css, examples, library)
  lazy val css = 
    Project("css", file("geocss"), settings = common)
  lazy val examples = 
    Project("examples", file("examples"), settings = common ++ Seq(fork in test := false, publish := false)) dependsOn(library)
  lazy val library =
    Project("library", file("geoscript"), settings = sphinxSettings ++ common) dependsOn(css)

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
