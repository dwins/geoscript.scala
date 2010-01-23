package org.geoscript

/**
 * The GeoScript trait serves as a convenience for loading all of geoscript
 * into a compiled Scala program.  Example usage:
 * <code>
 *   import org.geoscript._
 *   object Main extends GeoScript {
 *     val p = geometry.Point(1, 12);
 *   }
 * </code>
 */
trait GeoScript extends geometry.Implicits with projection.Implicits

/**
 * The GeoScript object provides a convenience for loading GeoScript into 
 * interpreted Scala scripts.  Example usage:
 * <code>
 *   import org.geoscript._
 *   import GeoScript._
 *   val p = geometry.Point(1, 12);
 * </code>
 */
object GeoScript extends GeoScript
