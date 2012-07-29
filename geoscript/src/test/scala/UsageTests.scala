package org.geoscript

import org.scalatest._, matchers._

import geometry._
import projection._

class UsageTests extends FunSuite with ShouldMatchers {
    test("work like on the geoscript homepage") { 
      var p = Point(-111, 45.7)
      var p2 = (Projection("epsg:4326") to Projection("epsg:26912"))(p)
      var poly = p.buffer(100)

      p2.x should be(closeTo(499999.0, 1))
      p2.y should be(closeTo(5060716.0, 0.5))
      poly.area should be(closeTo(31214.45, 0.01))
    }

    test("linestrings should be easy") { 
      LineString(
        (10.0, 10.0), (20.0, 20.0), (30.0, 40.0)
      ).length should be(closeTo(36.503, 0.001))

      LineString((10, 10), (20.0, 20.0), (30, 40))
        .length should be(closeTo(36.503, 0.001))
    }

    test("polygon should be easy") {
      Polygon(
        LineString((10, 10), (10, 20), (20, 20), (20, 15), (10, 10))
      ).area should be (75)
    }

    test("multi point should be easy") {
      MultiPoint((20, 20), (10.0, 10.0)).area should be (0)
    } 

    val states = getClass().getResource("/data/states.shp").toURI
    require(states.getScheme() == "file")
    val statesPath = new java.io.File(states)

    test("be able to read shapefiles") {
      val shp = layer.Shapefile(statesPath)
      shp.name should be ("states")
      shp.count should be (49)

      shp.envelope.getMinX should be(closeTo(-124.731422, 1d))
      shp.envelope.getMinY should be(closeTo(24.955967, 1d))
      shp.envelope.getMaxX should be(closeTo(-66.969849, 1d))
      shp.envelope.getMaxY should be(closeTo(49.371735, 1d))
      // proj should be ("EPSG:4326")
    }

    test("support search") {
      val shp = layer.Shapefile(statesPath)
      shp.features.find(_.id == "states.1") should be ('defined)
    }

    test("provide access to schema information") {
      val shp = layer.Shapefile(statesPath)
      shp.schema.name should be ("states")
      val field = shp.schema.get("STATE_NAME")
      field.name should be ("STATE_NAME")
      (field.gtBinding: AnyRef) should be (classOf[java.lang.String])
    }

    test("provide access to the containing workspace") {
      val shp = layer.Shapefile(statesPath)
      shp.workspace should not be(null)
    }

    test("provide a listing of layers") {
      val mem = workspace.Memory()
      mem.names should be ('empty)
    }

    test("allow creating new layers") {
      val mem = workspace.Memory()
      mem.names should be ('empty)
      var dummy = mem.create("dummy", 
        feature.Field("name", classOf[String]),
        feature.Field("geom", classOf[com.vividsolutions.jts.geom.Geometry], "EPSG:4326")
      )
      mem.names.length should be (1)

      dummy += feature.Feature(
        "name" -> "San Francisco",
        "geom" -> Point(37.78, -122.42)
      )

      dummy += feature.Feature(
        "name" -> "New York",
        "geom" -> Point(40.47, -73.58)
      )

      dummy.count should be (2)
      
      dummy.features.find(
        f => f.get[String]("name") == "New York"
      ) should be ('defined)
    }

  def closeTo(d: Double, eps: Double): BeMatcher[Double] =
    new BeMatcher[Double] {
      def apply(x: Double) = new MatchResult(
        math.abs(d - x) <= eps,
        "Value %f not within %f of expected value %f".format(x, eps, d),
        "Value %f was within %f of expected value %f".format(x, eps, d)
      )
    }
}
