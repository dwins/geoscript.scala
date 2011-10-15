name := "geocss"

libraryDependencies <++= gtVersion { v =>
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-cql" % v
  )
}

libraryDependencies +=
  "org.scala-tools.testing" %% "specs" % "[1.6,1.7)" % "test"
