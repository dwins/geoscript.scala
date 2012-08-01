package org.geoscript
package geometry

import org.scalatest._, matchers._

class SerializationSpec extends FunSuite with ShouldMatchers {
  test("round-trip points") {
    val p = point(100, 0)
    val json = GeoJSON.format(p)
    json should be("""{"type":"Point","coordinates":[100,0.0]}""")
    GeoJSON.read(json) should be(p)
    // TODO: Implement equality for geometries
  }

  test("round-trip linestrings") {
    val ls = lineString(Seq((100, 0), (101, 1)))
    GeoJSON.format(ls) should be(
      """{"type":"LineString","coordinates":[[100,0.0],[101,1]]}""")
  }

  test("round-trip polygons") {
    val solid = polygon(
      Seq((100, 0), (101, 0), (101, 1), (100, 1), (100, 0))
    )

    val withHoles = polygon(
      Seq((100, 0), (101, 0), (101, 1), (100, 1), (100, 0)),
      Seq(
        Seq((100.2, 0.2), (100.8, 0.2), (100.8, 0.8), (100.2, 0.8), (100.2, 0.2))
      )
    )

    GeoJSON.format(solid) should be(
      """{"type":"Polygon","coordinates":[[[100,0.0],[101,0.0],[101,1],[100,1],[100,0.0]]]}""")
    GeoJSON.format(withHoles) should be(
      """{"type":"Polygon","coordinates":[[[100,0.0],[101,0.0],[101,1],[100,1],[100,0.0]],[[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]]}""")
  }

  test("round-trip a multipoint") {
    val mp = multiPoint(Seq((100.0, 0.0), (101.0, 1.0)))
    GeoJSON.format(mp) should be(
      """{"type":"MultiPoint","coordinates":[[100,0.0],[101,1]]}""")
  }

  test("round-trip a MultiLineString") {
    val mls = multiLineString(Seq(
      Seq((100, 0), (101, 1)),
      Seq((102, 2), (103, 3))
    ))

    GeoJSON.format(mls) should be(
    """{"type":"MultiLineString","coordinates":[[[100,0.0],[101,1]],[[102,2],[103,3]]]}""")
  }

  test("round-trip a MultiPolygon") {
    val mp = multiPolygon(Seq(
      (Seq((102, 2), (103, 2), (103, 3), (102, 3), (102, 2)), Nil),
      (Seq((100, 0), (101, 0), (101, 1), (100, 1), (100, 0)),
        Seq(Seq((100.2, 0.2), (100.8, 0.2), (100.8, 0.8), (100.2, 0.8), (100.2, 0.2))))))

    GeoJSON.format(mp) should be(
      """{"type":"MultiPolygon","coordinates":[[[[102,2],[103,2],[103,3],[102,3],[102,2]]],[[[100,0.0],[101,0.0],[101,1],[100,1],[100,0.0]],[[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]]]}""")
  }

  test("round-trip a GeometryCollection") {
    val gc = multi(Seq(point(100, 0), lineString(Seq((101, 0), (102, 1)))))
    GeoJSON.format(gc) should be(
      """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0.0]},{"type":"LineString","coordinates":[[101,0.0],[102,1]]}]}""")
  }

  test("We should be able to stream JSON straight to a file") {
    val p = point(1, 2)
    GeoJSON.write(p, new java.io.File("/home/dwins/Blub.json"))
  }
}
