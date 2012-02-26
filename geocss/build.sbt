name := "geocss"

libraryDependencies <++= gtVersion { v =>
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-cql" % v
  )
}

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.7.1" % "test",
  "dwins" %% "filter-logic" % "0.2",
  "dwins" %% "logic" % "0.1"
)
