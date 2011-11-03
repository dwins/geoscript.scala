package org.geoscript
package workspace

import org.specs._
import com.vividsolutions.jts.geom.Point

class MemorySpec extends Specification {
  "Memory datastores" should {
    "be able to create layers" in {
      val schema = feature.Schema("cities",
        feature.Field("the_geom", classOf[Point], "EPSG:4326"),
        feature.Field("name", classOf[String])
      )
      val ws = workspace.Memory()
      val lyr = ws.create(schema)
      lyr += feature.Feature(
        "the_geom" -> geometry.Point(0, 0),
        "name" -> "test"
      )
      lyr.bounds must not(throwAn[Exception])
    }
  }
}
