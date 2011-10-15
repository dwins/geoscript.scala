name := "dummy"

publishArtifact := false

libraryDependencies += 
  "xml-apis" % "xml-apis-xerces" % "2.7.1" from "http://repo.opengeo.org/xml-apis/xml-apis-xerces/2.7.1/xml-apis-xerces-2.7.1.jar"

// update <<= update(x => error("Please ignore following error about OSGeo and OpenGeo repositories..."))
