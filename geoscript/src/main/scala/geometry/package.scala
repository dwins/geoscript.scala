package org.geoscript
package object geometry {
  import com.vividsolutions.jts.{geom => jts}

  type Geometry = jts.Geometry
  type GeometryCollection = jts.GeometryCollection
  type Polygon = jts.Polygon
  type MultiPolygon = jts.MultiPolygon
  type LinearRing = jts.LinearRing
  type LineString = jts.LineString
  type MultiLineString = jts.MultiLineString
  type Point = jts.Point
  type MultiPoint = jts.MultiPoint
  
  type Coordinate = jts.Coordinate
  type Envelope = jts.Envelope
  type Transform = jts.util.AffineTransformation

  val EmptyEnvelope = new jts.Envelope

  val factory = new jts.GeometryFactory

  def union(a: Envelope, b: Envelope): Envelope =
    if (a.isNull) b
    else if (b.isNull) a
    else {
      val res = new jts.Envelope(a)
      res.expandToInclude(b)
      res
    }

  def tupleAsCoordinate(xy: (Double, Double)): Coordinate =
    new jts.Coordinate(xy._1, xy._2)

  def coordinate(x: Double, y: Double): Coordinate =
    new jts.Coordinate(x, y)

  def envelope(minX: Double, maxX: Double, minY: Double, maxY: Double): Envelope =
    new jts.Envelope(minX, maxX, minY, maxY)
    
  def point(x: Double, y: Double): Point =
    factory.createPoint(coordinate(x, y))
  
  def lineString(coords: Seq[(Double, Double)]): Geometry =
    factory.createLineString(coords.map(tupleAsCoordinate).toArray)

  def linearRing(coords: Seq[(Double, Double)]): LinearRing =
    factory.createLinearRing(coords.map(tupleAsCoordinate).toArray)

  def polygon(
    shell: Seq[(Double, Double)],
    holes: Seq[Seq[(Double, Double)]] = Nil
  ): Geometry = 
    factory.createPolygon(
      linearRing(shell),
      holes.map(linearRing).toArray)

  def multiPoint(coords: Seq[(Double, Double)]): Geometry =
    factory.createMultiPoint(
      coords
        .map { case (x, y) => factory.createPoint(coordinate(x, y)) }
        .toArray
    )

  def multiLineString(strings: Seq[Seq[(Double, Double)]]): Geometry =
    factory.createMultiLineString(
      strings.map( xs =>
        factory.createLineString(
          xs.map(tupleAsCoordinate).toArray
        )
      ).toArray
    )

  def multiPolygon(
    polygons: Seq[(Seq[(Double, Double)], Seq[Seq[(Double, Double)]])]
  ): Geometry =
    factory.createMultiPolygon(
      polygons.map { case (shell, holes) =>
        factory.createPolygon(
          linearRing(shell),
          holes.map(linearRing).toArray)
      }.toArray
    )

  def multi(geoms: Seq[_ <: Geometry]): Geometry = {
    if (geoms.forall(_.isInstanceOf[Point]))
      factory.createMultiPoint(
        geoms.collect { case (p: Point) => p }.toArray)
    else if (geoms.forall(_.isInstanceOf[LineString]))
      factory.createMultiLineString(
        geoms.collect { case (ls: LineString) => ls }.toArray)
    else if (geoms.forall(_.isInstanceOf[Polygon]))
      factory.createMultiPolygon(
        geoms.collect { case (p: Polygon) => p }.toArray)
    else
      factory.createGeometryCollection(geoms.toArray)
  }

  def simplify(g: Geometry, tolerance: Double): Geometry =
    com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
      .simplify(g, tolerance)

  def Transform = new jts.util.AffineTransformation
}
