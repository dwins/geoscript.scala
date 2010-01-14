package org.geoscript

import com.vividsolutions.jts.{geom => jts}
import Stream._

trait GeoHash extends geometry.Implicits {
  private val characters = "0123456789bcdefghjkmnpqrstuvwxyz"

  def geohash(geom: jts.Geometry): String = {
    val bbox = geom.bounds
    val blHash = geohashForever(bbox.getMinY(), bbox.getMinX())
    val urHash = geohashForever(bbox.getMaxY(), bbox.getMaxX())

    (blHash zip urHash)
      .takeWhile({ case (x, y) => x == y })
      .take(32)
      .map(_._1)
      .mkString
  }

  def geohash(lat: Double, long: Double, level: Int): String =
    geohashForever(lat, long).take(level).mkString

  def decode(hash: String): (Double, Double) = {
    val center = decodeBounds(hash).centre
    (center.y, center.x)
  }

  def decodeBounds(hash: String): jts.Envelope = {
    val bits = hash.flatMap ((x: Char) => {
      val bitString = characters.indexOf(x).toBinaryString
      ("00000".substring(0, 5 - bitString.length) + bitString).map('1' ==)
    })

    val (lonBits, latBits) = separate(bits.toStream)
    val (minLon, maxLon) = range(lonBits, -180, 180)
    val (minLat, maxLat) = range(latBits,  -90,  90)

    new jts.Envelope(minLon, maxLon, minLat, maxLat)
  }

  private def geohashForever(lat: Double, long: Double): Stream[Char] =
    text(alternate(hash(long, -180, 180), hash(lat, -90, 90)))

  private def alternate[A](x: Stream[A], y: Stream[A]): Stream[A] =
    Stream.cons(x.head, alternate(y, x.tail))

  private def separate[A](combined: Stream[A]): (Stream[A], Stream[A]) =
    if (combined isEmpty) {
      (Stream.empty, Stream.empty) 
    } else {
      val (xs, ys) = separate(combined tail)
      (cons(combined.first, ys), xs)
    }

  private def hash(x:Double, min: Double, max: Double): Stream[Boolean] = {
    val mid = (min + max) / 2
    if (x >= mid) cons(true,  hash(x, mid, max))
    else          cons(false, hash(x, min, mid))
  }

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
