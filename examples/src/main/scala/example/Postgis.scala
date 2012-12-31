package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import feature.{ Feature, Field, GeoField, Schema }
import projection.Projection

object PostgisTest extends App { 
  val params = workspace.Params.postgis("conflict")

  workspace.withWorkspace(params) { conflict =>
    val fields = conflict("conflictsite").schema.fields
    
    for (field <- fields) println(field.name)

    workspace.withWorkspace(workspace.Params.postgis("test")) { wsTest =>
      val test = wsTest.create(Schema(
        "test",
        Seq(Field("name", classOf[String]),
            GeoField("geom", Projection("EPSG:4326"), classOf[Geometry]))))

      test += feature.fromAttributes(
        "name" -> "test",
        "geom" -> geometry.point(43,74)
      ) 
    }
  }
} 
