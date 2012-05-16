package org.geoscript
package workspace

import org.specs._

class MemorySpec extends Specification {
  "Memory datastores" should {
    "be able to create layers" in {
      val schema = feature.Schema("cities",
        feature.Field("the_geom", classOf[geometry.Point], "EPSG:4326"),
        feature.Field("name", classOf[String])
      )
      workspace.withMemoryWorkspace { ws => 
        val lyr = ws.create(schema)
        lyr += feature.Feature(
          "the_geom" -> geometry.point(0, 0),
          "name" -> "test"
        )
        lyr.envelope must not(throwAn[Exception])
      }
    }
  }
}
