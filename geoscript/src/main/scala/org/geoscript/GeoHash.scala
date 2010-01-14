package org.geoscript

import Stream._

trait GeoHash {
  private val characters = "0123456789bcdefghjkmnpqrstuvwxyz"

  def geohash(lat: Double, long: Double, size: Int): String = {
    val lonBits = hash(long, -180, 180)
    val latBits = hash(lat,   -90,  90)
    val bits = alternate(lonBits, latBits)

    text(bits).take(size).mkString
  }

  def decode(hash: String): (Double, Double) = {
    val bits = hash.flatMap ((x: Char) => {
      val bitString = characters.indexOf(x).toBinaryString
      ("00000".substring(0, 5 - bitString.length) + bitString).map('1' ==)
    })

    val (lonBits, latBits) = separate(bits.toStream)

    (coord(latBits, -90, 90), coord(lonBits, -180, 180))
  }

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
 
  private def coord(bits: Stream[Boolean], min: Double, max: Double): Double = {
    val mid = (min + max) / 2

    if (bits isEmpty)   max
    else if (bits.head) coord(bits.tail, mid, max) 
    else                coord(bits.tail, min, mid)
  }
} 
