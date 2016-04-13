name := "geoscript"

libraryDependencies +=
  "org.scala-lang" % "scala-swing" % "2.11.0-M7"

libraryDependencies <++= gtVersion { v => 
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-epsg-hsql" % v,
    "org.geotools" % "gt-shapefile" % v,
    "org.geotools" % "gt-render" % v,
    "org.geotools" % "gt-xml" % v,
    "org.geotools" % "gt-geojson" % v,
    "org.geotools.jdbc" % "gt-jdbc-postgis" % v,
    "org.geotools.jdbc" % "gt-jdbc-spatialite" % v
  )
}

libraryDependencies ++= 
  Seq(
    "javax.media" % "jai_core" % "1.1.3",
    "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    "com.lowagie" % "itext" % "2.1.5"
  )
