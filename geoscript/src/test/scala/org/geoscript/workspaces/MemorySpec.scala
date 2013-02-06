package org.geoscript
package workspace

import org.scalatest._, matchers._
import geometry._, geometry.builder._
import feature._, feature.schemaBuilder._
import projection._

class MemorySpec extends FunSuite with ShouldMatchers {
  test("be able to create layers") {
    val schema = Schema("cities",
      Seq(
        GeoField("the_geom", classOf[Point], LatLon),
        Field("name", classOf[String])))
    val ws = workspace.Memory()
    val lyr = ws.create(schema)
    lyr += Feature(
      "the_geom" -> Point(0, 0),
      "name" -> "test"
    )
    lyr.envelope should not be(null)
  }
}
