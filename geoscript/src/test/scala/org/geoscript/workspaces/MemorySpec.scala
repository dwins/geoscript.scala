package org.geoscript
package workspace

import org.scalatest._, matchers._
import com.vividsolutions.jts.geom.Point
import projection._

class MemorySpec extends FunSuite with ShouldMatchers {
  test("be able to create layers") {
    val schema = feature.Schema("cities",
      feature.Field("the_geom", classOf[Point], Projection("EPSG:4326")),
      feature.Field("name", classOf[String])
    )
    val ws = workspace.Memory()
    val lyr = ws.create(schema)
    lyr += feature.Feature(
      "the_geom" -> geometry.Point(0, 0),
      "name" -> "test"
    )
    lyr.envelope should not be(null)
  }
}
