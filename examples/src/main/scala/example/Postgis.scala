package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import feature.{ Feature, Field, Schema, bind }

object PostgisTest extends App { 
  val params = workspace.Params.postgis("conflict")

  workspace.withWorkspace(params) { conflict =>
    val fields = conflict.layerNamed("conflictsite").schema.fields
    
    for (field <- fields) println(field.name)

    workspace.withWorkspace(workspace.Params.postgis("test")) { wsTest =>
      val test = wsTest.create(Schema(
        "test",
        Seq(bind[String]("name"), bind[Geometry]("geom", "EPSG:4326"))))

      test += feature.fromAttributes(
        "name" -> "test",
        "geom" -> geometry.point(43,74)
      ) 
    }
  }
} 
