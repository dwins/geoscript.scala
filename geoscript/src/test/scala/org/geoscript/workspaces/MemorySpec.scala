package org.geoscript
package workspace

import projection.Projection, feature.bind

import org.scalatest._, matchers._
import com.vividsolutions.jts.geom.Point

class MemorySpec extends FunSuite with ShouldMatchers {
  test("be able to create layers") {
    val schema = feature.Schema("cities",
      Seq(
        bind[geometry.Point]("the_geom", Projection("EPSG:4326")),
        bind[String]("name")))
    val ws = workspace.Memory()
    val lyr = ws.create(schema)
    lyr += feature.fromAttributes(
      "the_geom" -> geometry.point(0, 0),
      "name" -> "test"
    )
    lyr.envelope should not be(null)
  }
}
