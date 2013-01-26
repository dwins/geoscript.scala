package org.geoscript
package geometry

import org.geoscript.io.{ Sink, Source }
import org.scalatest._, matchers._

class SerializationSpec extends FunSuite with ShouldMatchers {
  test("round-trip points") {
    val p = Point(100, 0)
    val json = io.GeoJSON.write(p, Sink.string)
    json should be("""{"type":"Point","coordinates":[100,0.0]}""")
    // io.GeoJSON.read(Source.string(json)) should be p
    // TODO: Implement equality for geometries
  }

  test("round-trip linestrings") {
    val ls = LineString((100, 0), (101, 1))
    io.GeoJSON.write(ls, Sink.string) should be (
      """{"type":"LineString","coordinates":[[100,0.0],[101,1]]}""")
  }

  test("round-trip polygons") {
    val solid = Polygon(
      LineString((100, 0), (101, 0), (101, 1), (100, 1), (100, 0))
    )

    val withHoles = Polygon(
      LineString((100, 0), (101, 0), (101, 1), (100, 1), (100, 0)),
      Seq(LineString(
        (100.2, 0.2), (100.8, 0.2), (100.8, 0.8), (100.2, 0.8), (100.2, 0.2)
      ))
    )

    io.GeoJSON.write(solid, Sink.string) should be(
      """{"type":"Polygon","coordinates":[[[100,0.0],[101,0.0],[101,1],[100,1],[100,0.0]]]}""")
    io.GeoJSON.write(withHoles, Sink.string) should be(
      """{"type":"Polygon","coordinates":[[[100,0.0],[101,0.0],[101,1],[100,1],[100,0.0]],[[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]]}""")
  }

  test("round-trip a multipoint") {
    val mp = MultiPoint((100.0, 0.0), (101.0, 1.0))
    io.GeoJSON.write(mp, Sink.string) should be(
      """{"type":"MultiPoint","coordinates":[[100,0.0],[101,1]]}""")
  }

  test("round-trip a MultiLineString") {
    val mls = MultiLineString(
      LineString((100, 0), Point(101, 1)),
      LineString((102, 2), Point(103, 3))
    )

    io.GeoJSON.write(mls, Sink.string) should be(
    """{"type":"MultiLineString","coordinates":[[[100,0.0],[101,1]],[[102,2],[103,3]]]}""")
  }

  test("round-trip a MultiPolygon") {
    val mp = MultiPolygon(
      Polygon(LineString(
        (102, 2), (103, 2), (103, 3), (102, 3), (102, 2)
      )),
      Polygon(LineString(
          (100, 0), (101, 0), (101, 1), (100, 1), (100, 0)
        ),
        Seq(LineString(
          (100.2, 0.2), (100.8, 0.2), (100.8, 0.8), (100.2, 0.8), (100.2, 0.2)
        ))
      )
    )

    io.GeoJSON.write(mp, Sink.string) should be(
      """{"type":"MultiPolygon","coordinates":[[[[102,2],[103,2],[103,3],[102,3],[102,2]]],[[[100,0.0],[101,0.0],[101,1],[100,1],[100,0.0]],[[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]]]}""")
  }

  test("round-trip a GeometryCollection") {
    val gc = GeometryCollection(
      Point(100.0, 0.0),
      LineString((101.0, 0.0), (102.0, 1.0))
    )

    io.GeoJSON.write(gc, Sink.string) should be(
      """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0.0]},{"type":"LineString","coordinates":[[101,0.0],[102,1]]}]}""")
  }
}
