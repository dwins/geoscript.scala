package org.geoscript

import org.specs._

import geometry._
import layer._
import projection._

class UsageTests extends Specification with GeoScript {
  "geometries should be quick and easy" in {
    var p = Point(-111, 45.7)
    var p2 = Projection("epsg:4326") to Projection("epsg:26912") apply p
    var poly = p.buffer(100)

    p2.x must beCloseTo(499999.0, 0.5)
    p2.y must beCloseTo(5060716.0, 0.5)
    poly.area must beCloseTo(31214.45, 0.01)
  }

  "Layers" should {
    val statesPath = "geoscript/src/test/resources/data/states.shp"
    "be able to read shapefiles" in {
      var shp = Shapefile(statesPath)
      val (xMin, yMin, xMax, yMax, proj) = shp.bounds

      shp.name must_== "states"
      shp.count must_== 49

      xMin must beCloseTo (-124.731422, 1d)
      yMin must beCloseTo (24.955967, 1d)
      xMax must beCloseTo (-66.969849, 1d)
      yMax must beCloseTo (49.371735, 1d)
      proj must_== "EPSG:4326"
    }

    "support iteration" in {
      var shp = Shapefile(statesPath)
      val features = for (f <- shp.features) yield f
      features.toStream.take(1).head.getIdentifier().toString() must_== 
        "states.1"
    }
  }
}
