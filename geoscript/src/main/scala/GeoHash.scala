package org.geoscript

import Stream._

/**
 * GeoHash provides some methods for encoding and decoding point information
 * with arbitrary precision according to the GeoHash scheme.
 *
 * @see http://en.wikipedia.org/wiki/Geohash
 */
object GeoHash {
  private val characters = "0123456789bcdefghjkmnpqrstuvwxyz"

  /**
   * Hash a JTS Geometry. This produces the most precise geohash that
   * corresponds to the corners of the geometry's envelope, with a cutoff at 32
   * characters to avoid generating infinitely strings for point data.
   */
  def geohash(geom: geometry.Geometry): String = {
    val bbox = geom.envelope
    val blHash = geohashForever(bbox.minY, bbox.minX)
    val urHash = geohashForever(bbox.maxY, bbox.maxX)

    (blHash zip urHash)
      .takeWhile({ case (x, y) => x == y })
      .take(32)
      .map(_._1)
      .mkString
  }

  /**
   * Generate a hash for a specific latitude/longitude pair. 
   * 
   * @param lat: the latitude
   * @param long: the longitude
   * @param level: how many characters of geohash to produce (ie, how much
   *     precision to use in the encoding)
   */
  def geohash(lat: Double, long: Double, level: Int): String =
    geohashForever(lat, long).take(level).mkString

  /**
   * Decode a geohash, producing the latitude/longitude pair that was
   * originally hashed (within precision)
   */
  def decode(hash: String): (Double, Double) = {
    val center = decodeBounds(hash).centre
    (center.y, center.x)
  }

  /**
   * Decode a geohash, producing a JTS Envelope encompassing the range of
   * possible values for the input point.
   */
  def decodeBounds(hash: String): geometry.Envelope = {
    val bits = hash.flatMap {(x: Char) => 
      val bitString = characters.indexOf(x).toBinaryString
      ("00000".substring(0, 5 - bitString.length) + bitString).map('1' ==)
    }

    val (lonBits, latBits) = separate(bits.toStream)
    val (minLon, maxLon) = range(lonBits, -180, 180)
    val (minLat, maxLat) = range(latBits,  -90,  90)

    geometry.Envelope(minLon, maxLon, minLat, maxLat)
  }

  /**
   * Generate an infinite stream of geohash characters for a particular
   * point.
   */
  private def geohashForever(lat: Double, long: Double): Stream[Char] =
    text(alternate(hash(long, -180, 180), hash(lat, -90, 90)))

  /**
   * Create a stream that alternates between the elements of the two input
   * streams.
   */
  private def alternate[A](x: Stream[A], y: Stream[A]): Stream[A] =
    Stream.cons(x.head, alternate(y, x.tail))

  /**
   * Untangle the elements of streams combined using alternate()
   */
  private def separate[A](combined: Stream[A]): (Stream[A], Stream[A]) =
    if (combined isEmpty) {
      (Stream.empty, Stream.empty) 
    } else {
      val (xs, ys) = separate(combined tail)
      (cons(combined.head, ys), xs)
    }

  /**
   * Get the bitwise stream of a hash.  Combine with text() to get geohash
   * characters.
   */
  private def hash(x:Double, min: Double, max: Double): Stream[Boolean] = {
    val mid = (min + max) / 2
    if (x >= mid) cons(true,  hash(x, mid, max))
    else          cons(false, hash(x, min, mid))
  }

  /**
   * Convert a Stream[Boolean], piecewise, into characters.  For use with
   * hash()
   */
  private def text(bits: Stream[Boolean]): Stream[Char] = {
    val char = bits.take(5).foldLeft(0) { (accum, bit) => 
      if (bit) (2 * accum) + 1 
      else      2 * accum
    }

    cons(characters(char), text(bits drop 5))
  }
 
  private def range(bits: Stream[Boolean], min: Double, max: Double)
  : (Double, Double) = 
  {
    lazy val mid = (min + max) / 2

    if (bits isEmpty)   (min, max)
    else if (bits.head) range(bits.tail, mid, max) 
    else                range(bits.tail, min, mid)
  }
} 
