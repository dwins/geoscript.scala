name := "geocss"

libraryDependencies <++= gtVersion { v =>
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-cql" % v
  )
}

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.7.1" % "test",
  "dwins" %% "filter-logic" % "0.3",
  "dwins" %% "logic" % "0.2"
)

initialCommands += """
import org.geoscript.geocss._
import collection.JavaConversions._
val kb = dwins.logic.Knowledge.Oblivion(SelectorsAreSentential)
def in(path: String) = new java.io.FileReader(new java.io.File(path))
def load(path: String) = CssParser.parseAll(CssParser.styleSheet, in(path)).get
val tx = new Translator()
"""
