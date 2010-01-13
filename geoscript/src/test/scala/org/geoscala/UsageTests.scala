package org.geoscript

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import geometry._
import projection._

class UsageTests extends GeoScript with FlatSpec with ShouldMatchers {
  "geometries" should "support buffering" in {
    var p = Point(-111, 45.7)
    var p2 = Projection("epsg:4326") to Projection("epsg:26912") apply p
    var poly = p.buffer(100)

    p2.x should be ( 500000.0 plusOrMinus 1)
    p2.y should be (5060716.0 plusOrMinus 1)
    poly.area should be (31214.45 plusOrMinus 0.01)
  }
}

