package tutorial

object BasicGeometry extends App {
  import org.geoscript.geometry
  // constructing geometries
  geometry.point(30, 10)
  geometry.lineString(Seq((30, 10), (10, 30), (20, 40), (40, 40)))
  geometry.polygon(Seq((30,10), (10,20), (20,40), (40,40), (30,10)))
  geometry.polygon(
    Seq((35,10), (10,20), (15,40), (45,45), (35,10)),
    Seq(Seq((20,30), (35,35), (30,20), (20,30))))
  geometry.multiPoint(Seq((10,40), (40,30), (20,20), (30,10)))
  geometry.multi(Seq(
    geometry.point(10,40),
    geometry.point(40,30),
    geometry.point(20,20),
    geometry.point(30,10)))
  geometry.multiLineString(Seq(
    Seq((10,10), (20,20), (10,40)),
    Seq((40,40), (30,30), (40,20), (30,10))))
  geometry.multiPolygon(Seq(
    (Seq((30,20), (10,40), (45,40), (30,20)), Nil),
    (Seq((15,5), (40,10), (10,20), (5,10), (15,5)), Nil)))

  // operations: measurement
  val poly =
    geometry.polygon(Seq((30, 10), (10, 20), (20, 40), (40, 40), (30, 10)))
  poly.getArea

  val line = 
    geometry.lineString(Seq((30, 10), (10, 20), (20, 40), (40, 40), (30, 10)))
  line.getLength

  poly.isValid

  geometry.polygon(Seq((1,1), (2,1), (1,0), (2,0), (1,1))).isValid

  // operations: relationships
  val point = geometry.point(30, 10)
  point.distance(geometry.point(40, 30))

  line.distance(point)
  line.intersects(point)
  line.intersection(point)

  val polyA = geometry.polygon(Seq((0,0), (2,0), (2,2), (0,2), (0,0)))
  val polyB = geometry.polygon(Seq((1,1), (3,1), (3,3), (1,3), (1,1)))
  polyA.difference(polyB)
  polyA.symDifference(polyB)

  // operations: derived geometries
  line.buffer(1)

  // serialization
  geometry.WKT.format(point)
  geometry.GeoJSON.format(point)

  // deserialization
  geometry.WKT.read("POINT (30 10)")
  geometry.GeoJSON.read("POINT (30 10)")
}
