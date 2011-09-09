package org.geoscript.geometry

import com.vividsolutions.jts.{geom=>jts}
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geoscript.projection.Projection

/**
 * The ModuleInternals object contains some state shared among the objects in
 * the geometry package.  It is not really intended for use by client code, it
 * just avoids creating multiple GeometryFactories for the different geometry
 * types and such.
 */
private object ModuleInternals {
  val factory = new jts.GeometryFactory() 

  /**
   * Try really hard to make objects into JTS Coordinates.  This function
   * accepts (so that it can be used to map mixed collections) so watch out for
   * runtime errors.
   * 
   * Convertible types include: 
   * <ul>
   *   <li> (Number, Number) </li>
   *   <li> (Number, Number, Number) </li>
   *   <li> com.vividsolutions.jts.geometry.Point </li>
   *   <li> com.vividsolutions.jts.geometry.Coordinate </li>
   * </ul>
   *
   * @param input: Any An object that is hopefully kind of like a Coordinate
   *     tuple.
   * @return the equivalent JTS Coordinate
   * @throws RuntimeException if the input is not convertible
   */
  def coerceCoord(input: Any) = {
    input match {
      case (x: Number, y: Number) => 
        new jts.Coordinate(x.doubleValue(), y.doubleValue())
      case (x: Number, y: Number, z: Number) =>
        new jts.Coordinate(x.doubleValue(), y.doubleValue(), z.doubleValue())
      case (p: jts.Point) => p.getCoordinate()
      case (p: Point) => p.underlying.getCoordinate()
      case (coord: jts.Coordinate) => coord
      case other => throw new RuntimeException(
        "Coordinates can only be coerced from numeric tuples or points; " + 
        "found %s [class %s] instead.".format(other, (other.asInstanceOf[AnyRef]).getClass)
      )
    }
  }
}

/**
 * An enumeration of the valid end-cap styles when buffering a (line) Geometry.
 * Valid styles include: 
 * <ul>
 *   <li>Round - A semicircle </li>
 *   <li>Butt - A straight line perpendicular to the end segment</li>
 *   <li>Square - A half-square</li>
 * </ul>
 * 
 * @see org.geoscript.geometry.Geometry.buffer
 */
object EndCap {
  import com.vividsolutions.jts.operation.buffer.BufferOp._

  sealed abstract class Style { val intValue: Int }
  /** @see EndCap */
  case object Butt extends Style { val intValue = CAP_BUTT }
  /** @see EndCap */
  case object Round extends Style { val intValue = CAP_ROUND }
  /** @see EndCap */
  case object Square extends Style { val intValue = CAP_SQUARE }
}

/**
 * The Geometry object contains several methods for dealing with Geometry
 * objects, including translating from JTS Geometries to GeoScript Geometries.
 */
object Geometry {
  val typeMapping = List(
    classOf[Point] -> classOf[jts.Point],
    classOf[Polygon] -> classOf[jts.Polygon],
    classOf[LineString] -> classOf[jts.LineString],
    classOf[MultiPoint] -> classOf[jts.MultiPoint],
    classOf[MultiPolygon] -> classOf[jts.MultiPolygon],
    classOf[MultiLineString] -> classOf[jts.MultiLineString],
    classOf[GeometryCollection] -> classOf[jts.GeometryCollection]
  )

  /**
   * Create a GeoScript Geometry equivalent to a given JTS Geometry.
   */
  implicit def apply(geom: jts.Geometry): Geometry = {
    geom match {
      case (geom: jts.Point) => Point(geom)
      case (geom: jts.Polygon) => Polygon(geom)
      case (geom: jts.LineString) => LineString(geom)
      case (geom: jts.MultiPoint) => MultiPoint(geom)
      case (geom: jts.MultiPolygon) => MultiPolygon(geom)
      case (geom: jts.MultiLineString) => MultiLineString(geom)
      case (geom: jts.GeometryCollection) => GeometryCollection(geom)
    }
  }

  implicit def unwrap(geom: Geometry): jts.Geometry = geom.underlying

  /** 
   * Find the class binding that should be used in the underlying GeoTools
   * methods when dealing with a particular GeoScript geometry class.  Note
   * that if you just want to unwrap a Geometry instance, you can simple use
   * geom.underlying.
   */
  def jtsClass(geomClass: Class[_ <: Geometry]): Class[_ <: jts.Geometry] =
    typeMapping find { 
      _._1.isAssignableFrom(geomClass) 
    } map { 
      _._2 
    } getOrElse classOf[jts.Geometry]

  /**
   * Find the class binding that should be used in GeoScript wrapper objects
   * for the given Geometry.  Note that if you just want to wrap an existing
   * JTS Geometry instance, you can simply use Geometry.apply().
   */
  def wrapperClass(geomClass: Class[_ <: jts.Geometry]): Class[_ <: Geometry] =
    typeMapping find {
      _._2.isAssignableFrom(geomClass)
    } map {
      _._1
    } getOrElse classOf[Geometry]

  /**
   * Create a GeoScript Geometry equivalent to a given JTS Geometry and
   * annotated with a Projection.  The coordinates of the original Geometry are
   * assumed to be in the provided projection.
   */
  def apply(geom: jts.Geometry, proj: Projection): Geometry =
    apply(geom) in proj
}

/**
 * The Geometry trait provides a common parent type for types representing
 * specific geometric entities such as points and line strings.  All GeoScript
 * geometry classes wrap their equivalent Geometry type from the Java Topology
 * Suite (JTS).
 *
 * @see com.vividsolutions.jts.geom.Geometry
 */
trait Geometry {

  /**
   * The JTS geometry wrapped by this Geometry instance.  Note that projection
   * information is not preserved if you manipulate the geometry directly.
   */
  val underlying: jts.Geometry

  /**
   * The area enclosed by this geometry, in the same units as used by its
   * coordinates.
   */
  def area: Double = underlying.getArea()

  /**
   * A jts.Envelope that fully encloses this Geometry.
   * @todo This should use a type from the GeoScript geometry package instead
   */
  def bounds: Bounds = Bounds(underlying.getEnvelopeInternal()) in projection

  /**
   * A point that represents the "center of gravity" of this geometry's
   * enclosed area.  Note that this point is not necessarily on the geometry!
   */
  def centroid: Point = Point(underlying.getCentroid()) in projection

  /**
   * All the coordinates that compose this Geometry as a sequence.
   */
  def coordinates: Seq[Point] = 
    underlying.getCoordinates() map (c => Point(c) in projection)

  /**
   * The length of the line segments that compose this geometry, in the same
   * units as used by its coordinates.
   */
  def length: Double = underlying.getLength()

  /**
   * A string representation of this Geometry in GeoJSON format
   * @see http://geojson.org/
   */
  def json: String = "" // TODO: Real JSON encoding
  
  /**
   * A string representation of this Geometry in WKT (Well-Known Text) format.
   * @see http://en.wikipedia.org/wiki/Well-known_text
   */
  def wkt: String = underlying.toString()

  /**
   * The Projection used for this Geometry's coordinates, or null if that is
   * unspecified.
   */
  def projection: Projection = null

  /**
   * Tests whether this geometry is "prepared," optimized for certain
   * spatial queries.
   *
   * @see prepare
   */
  def prepared: Boolean = false

  /**
   * Creates a prepared geometry equivalent to this one.  Prepared geometries
   * are slower to create, but provide faster implementations of various
   * spatial operations.
   */
  def prepare(): Geometry
  
  /**
   * Create a new Geometry expanding a set distance out from the boundaries of
   * this one.
   *
   * @param dist: The distance to expand out.  May be positive, 0, or negative.
   * @param segs: The number of line segments to create at vertices
   * @param mode: The style to use for endcaps when buffering linear geometries
   * @see EndCap
   */
  def buffer(dist: Double, segs: Int = 8, mode: EndCap.Style = EndCap.Round)
  : Geometry = 
    Geometry(underlying.buffer(dist, segs, mode.intValue), projection)

  /**
   * Create a new Geometry equivalent to this one, but in the specified
   * Projection.  If the projection for this geometry is known, this involves 
   * transforming the coordinates; otherwise the projection is merely
   * associated with the existing coordinates.
   */
  def in(proj: Projection): Geometry

  /**
   * Determines whether this Geometry contains any common points with the
   * provided one.
   */
  def intersects(that: Geometry): Boolean = 
    this.underlying intersects that.underlying

  /**
   * Create a new Geometry which contains only the areas included by both this
   * Geometry and the one passed as an argument.
   * 
   * @todo Account for projection differences here
   */
  def intersection(that: Geometry): Geometry = 
    Geometry(underlying intersection that.underlying)

  /**
   * Create a new Geometry which contains only the areas included by either this
   * Geometry or the one passed as an argument.
   * 
   * @todo Account for projection differences here
   */
  def union(that: Geometry): Geometry = 
    Geometry(underlying union that.underlying)

  def mapVertices(op: Point => Point): Geometry = {
    val geom = underlying.clone().asInstanceOf[jts.Geometry]
    geom.apply( new jts.CoordinateFilter {
      def filter(coord: jts.Coordinate) = op(Point(coord)).underlying
    })
    Geometry(geom)
  }

  /**
   * Are the coordinates of this geometry in an acceptable order? (no
   * self-intersecting polygons, etc.)
   */
  def isValid: Boolean = underlying.isValid

  /**
   * Like <code>in</code>, but fails if this Geometry doesn't have a projection
   * set.  
   *
   * @see in
   */
  def transform(dest: Projection): Geometry 

  override def toString = underlying.toString
} 
