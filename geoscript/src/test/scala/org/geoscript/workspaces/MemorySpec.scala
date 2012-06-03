package org.geoscript
package workspace

import org.specs._

class MemorySpec extends Specification {
  import feature.{ Schema, bind }
  "Memory datastores" should {
    "be able to create layers" in {
      val schema = Schema("cities",
        Seq(
          bind[geometry.Point]("the_geom", "EPSG:4326"),
          bind[String]("name")))
      workspace.withMemoryWorkspace { ws => 
        val lyr = ws.create(schema)
        lyr += feature.fromAttributes(
          "the_geom" -> geometry.point(0, 0),
          "name" -> "test"
        )
        lyr.envelope must not(throwAn[Exception])
      }
    }
  }
}
