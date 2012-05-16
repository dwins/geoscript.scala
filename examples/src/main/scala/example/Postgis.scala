package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import feature.{ Feature, Field }

object PostgisTest extends App { 
  val params = workspace.Params.postgis("conflict")

  workspace.withWorkspace(params) { conflict =>
    val fields = conflict.layerNamed("conflictsite").schema.fields
    
    for (field <- fields) println(field.name)

    workspace.withWorkspace(workspace.Params.postgis("test")) { wsTest =>
      val test = wsTest.create(feature.Schema("test",
        Field("name", classOf[String]),
        Field("geom", classOf[Geometry], "EPSG:4326")
      ))

      test += Feature( 
        "name" -> "test",
        "geom" -> geometry.point(43,74)
      ) 
    }
  }
} 
