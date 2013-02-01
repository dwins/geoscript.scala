package org.geoscript.example

import org.geoscript.feature._
import org.geoscript.geometry._
import org.geoscript.geometry.builder._
import org.geoscript.projection._
import org.geoscript.workspace._

object PostgisTest extends App { 
  val conflict = Postgis("database" -> "conflict")
  val fields = conflict.layer("conflictsite").schema.fields
  
  for (field <- fields) println(field.name)
  val workSpaceTest = Postgis() 
  
  val test = workSpaceTest.create("test",
    Field("name", classOf[String]),
    Field("geom", classOf[Geometry], lookupEPSG("EPSG:4326").get)
  )

  test += Feature("name" -> "test", "geom" -> Point(43,74)) 
} 
