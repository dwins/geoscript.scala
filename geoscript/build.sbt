name := "geoscript"

libraryDependencies <++= gtVersion { v => 
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-epsg-hsql" % v,
    "org.geotools" % "gt-shapefile" % v,
    "org.geotools" % "gt-render" % v,
    "org.geotools" % "gt-xml" % v,
    "org.geotools" % "gt-geojson" % v,
    "org.geotools" % "gt-charts" % v,
    "org.geotools.jdbc" % "gt-jdbc-postgis" % v,
    "org.geotools.jdbc" % "gt-jdbc-spatialite" % v
  )
}

libraryDependencies ++= Seq(
  "javax.media" % "jai_core" % "1.1.3",
  "org.scala-tools.testing" % "specs_2.9.1" % "[1.6,1.7)" % "test",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "com.lowagie" % "itext" % "2.1.5")
