package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import layer._

object PostgisTest extends GeoScript { 
  def main(args: Array[String]) {
    val conflict = workspace.Postgis("database" -> "conflict")
    val fields = conflict.layer("conflictsite").schema.fields
    
     for (field <- fields) { 
        println(field.name) 
        } 

  }
} 
