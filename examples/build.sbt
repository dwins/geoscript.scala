name := "examples"

organization := "org.geoscript"

version := "0.7.1"

resolvers ++= Seq(
  "OSGeo" at "http://download.osgeo.org/webdav/geotools/",
  "OpenGeo" at "http://repo.opengeo.org/"
)

libraryDependencies <+= 
  (organization, version) apply { _ %% "geoscript" % _ }
