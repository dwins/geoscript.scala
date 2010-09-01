package org.geoscript

/**
 * The GeoScript trait serves as a convenience for loading all of GeoScript
 * into a compiled Scala program.  Example usage:
 * <pre>
 * import org.geoscript._
 * object Main extends GeoScript {
 *   def main(args: Array[String]) {
 *     val p = geometry.Point(1, 12)
 *   }
 * }
 * </pre>
 */
trait GeoScript extends geometry.Implicits

/**
 * The GeoScript object provides a convenience for loading GeoScript into 
 * interpreted Scala scripts.  Example usage:
 * <pre>
 * import org.geoscript._
 * import GeoScript._
 * val p = geometry.Point(1, 12);
 * </pre>
 */
object GeoScript extends GeoScript
