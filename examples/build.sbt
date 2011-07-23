name := "examples"

organization := "org.geoscript"

version := "0.7.1"

resolvers ++= Seq(
  "OSGeo" at "http://download.osgeo.org/webdav/geotools/",
  "OpenGeo" at "http://repo.opengeo.org/"
)

libraryDependencies +=
    "net.sf.json-lib" % "json-lib" % "2.3" classifier "jdk15"

libraryDependencies <+= 
  (organization, version) apply { _ %% "geoscript" % _ }
