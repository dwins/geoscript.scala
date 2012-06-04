// Enable OSGeo repository for GeoTools and its dependencies
resolvers += "osgeo" at "http://download.osgeo.org/webdav/geotools/"

// Enable OpenGeo repository for GeoScript itself
resolvers += "opengeo" at "http://repo.opengeo.org/"

// list GeoScript as a dependency for the project
libraryDependencies +=
   "org.geoscript" %% "geoscript" % "0.7.3"

// Currently GeoScript is only built for Scala version 2.9.1
scalaVersion := "2.9.1"
