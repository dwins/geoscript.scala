package org.geoscript
package workspace

import feature._, projection._

import org.scalatest._, matchers._
import com.vividsolutions.jts.geom.Point

class MemorySpec extends FunSuite with ShouldMatchers {
  test("be able to create layers") {
    val schema = Schema("cities",
        // (setProjection("the_geom".binds[geometry.Point], LatLon) ~ 
        ("the_geom".binds[geometry.Point] ~ 
        "name".binds[String]))
    val ws = workspace.Memory()
    val lyr = ws.create(schema)
    lyr += feature.fromAttributes(
      "the_geom" -> geometry.point(0, 0),
      "name" -> "test"
    )
    lyr.envelope should not be(null)
  }
}
