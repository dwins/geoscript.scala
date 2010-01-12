package org.geoscript

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import geometry.Implicits._

class UsageTests extends FlatSpec with ShouldMatchers {
  "geometries" should "support buffering" in {
    var p = geometry.Point(-111, 45.7)
    // var p2 = projection.transform(p, "epsg:4326", "epsg:26912")
    var poly = p.buffer(100)
    poly.area should be (31214.45 plusOrMinus 0.01)
  }
}

