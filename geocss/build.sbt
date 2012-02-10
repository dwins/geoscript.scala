name := "geocss"

libraryDependencies <++= gtVersion { v =>
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-cql" % v
  )
}

libraryDependencies +=
  "org.specs2" %% "specs2" % "1.7.1" % "test"
