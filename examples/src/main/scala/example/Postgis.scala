package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import feature.{ Feature, Field }

object PostgisTest extends App { 
  val conflict = workspace.Postgis("database" -> "conflict")
  val fields = conflict.layer("conflictsite").schema.fields
  
  for (field <- fields) println(field.name)
  val workSpaceTest = workspace.Postgis() 
  
  val test = workSpaceTest.create("test",
    Field("name", classOf[String]),
    Field("geom", classOf[Geometry], "EPSG:4326")
  )

  test += Feature( 
    "name" -> "test",
    "geom" -> geometry.Point(43,74)
  ) 
} 
