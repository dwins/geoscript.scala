package org.geoscript.geometry

import org.specs._
import org.geoscript.GeoScript._

class SerializationSpec extends Specification {
  "JSON Serialization" should {
    "round-trip points" in {
      val p = Point(100, 0)
      Geometry.toJSON(p) must_== """{"type":"Point","coordinates":[100,0]}"""
      // Geometry.fromJSON(Geometry.toJSON(p)) must_== p
    }

    "round-trip linestrings" in {
      val ls = LineString(Point(100, 0), Point(101, 1))
      Geometry.toJSON(ls) must_==
        """{"type":"LineString","coordinates":[[100,0],[101,1]]}"""
    }

    "round-trip polygons" in {
      val solid = Polygon(
        LineString(Seq(
          Point(100, 0), Point(101, 0), Point(101, 1), Point(100, 1),
          Point(100, 0)
        )), 
        Nil
      )

      val withHoles = Polygon(
        LineString(Seq(
          Point(100, 0), Point(101, 0), Point(101, 1), Point(100, 1),
          Point(100, 0))
        ),
        Seq(
          LineString(Seq(
            Point(100.2, 0.2), Point(100.8, 0.2),
            Point(100.8, 0.8), Point(100.2, 0.8),
            Point(100.2, 0.2)
          ))
        )
      )

      Geometry.toJSON(solid) must_==
        """{"type":"Polygon","coordinates":[[[100,0],[101,0],[101,1],[100,1],[100,0]]]}"""
      Geometry.toJSON(withHoles) must_==
        """{"type":"Polygon","coordinates":[[[100,0],[101,0],[101,1],[100,1],[100,0]],[[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]]}"""
    }

    "round-trip a multipoint" in {
      val mp = MultiPoint(Seq((100.0, 0.0), (101.0, 1.0)))
      Geometry.toJSON(mp) must_==
        """{"type":"MultiPoint","coordinates":[[100,0],[101,1]]}"""
    }

    "round-trip a MultiLineString" in {
      val mls = MultiLineString(
        LineString(Point(100, 0), Point(101, 1)),
        LineString(Point(102, 2), Point(103, 3))
      )

      Geometry.toJSON(mls) must_== 
        """{"type":"MultiLineString","coordinates":[[[100,0],[101,1]],[[102,2],[103,3]]]}"""
    }

    "round-trip a MultiPolygon" in {
      val mp = MultiPolygon(
        Polygon(
          LineString(
            Point(102.0, 2.0), Point(103.0, 2.0), Point(103.0, 3.0), Point(102.0, 3.0),
            Point(102.0, 2.0)
          ),
          Seq.empty
        ),
        Polygon(
          LineString(
            Point(100.0, 0.0), Point(101.0, 0.0), Point(101.0, 1.0), Point(100.0, 1.0), 
            Point(100.0, 0.0)
          ),
          Seq(
            LineString(
              Point(100.2, 0.2), Point(100.8, 0.2), Point(100.8, 0.8), Point(100.2, 0.8), 
              Point(100.2, 0.2)
            )
          )
        )
      )

      Geometry.toJSON(mp) must_== 
        """{"type":"MultiPolygon","coordinates":[[[[102,2],[103,2],[103,3],[102,3],[102,2]]],[[[100,0],[101,0],[101,1],[100,1],[100,0]],[[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]]]}"""
    }

    "round-trip a GeometryCollection" in {
      val gc = GeometryCollection(
        Point(100.0, 0.0),
        LineString(Point(101.0, 0.0), Point(102.0, 1.0))
      )

      Geometry.toJSON(gc) must_==
        """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]}]}"""
    }
  }
}
