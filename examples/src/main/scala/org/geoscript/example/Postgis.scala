package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._

object PostgisTest extends GeoScript { 
  def main(args: Array[String]) {
    val conflict = workspace.Postgis("database" -> "conflict")
    val fields = conflict.layer("conflictsite").schema.fields
    
     for (field <- fields) { 
        println(field.name) 
        } 
    val workSpaceTest = workspace.Postgis() 
    
    val test = workSpaceTest.create("test",
        feature.Field("name", classOf[String]),
        feature.Field("geom", classOf[com.vividsolutions.jts.geom.Geometry])
    )

    test += feature.Feature( 
      "name" -> "test",
      "geom" -> geometry.Point(43,74)
    ) 
  }
} 
