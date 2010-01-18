package org.geoscript

import org.specs._

class GeoHashTest extends Specification with GeoHash {
  val cases = Seq(
    (57.64911, 10.40744, 11, "u4pruydqqvj"),
    (42.6, -5.6, 5, "ezs42")
  )

  "The GeoHash examples from Wikipedia" should {
    "produce the cited hashes" in {
      cases.foreach { case (lon, lat, level, hash) => 
        geohash(lon, lat, level) must_== (hash)
      }
    }

    "work in reverse" in {
      cases.foreach { case (lon, lat, level, hash) => 
        val (actualLon, actualLat) = decode(hash)
        actualLon must beCloseTo(lon, 0.005)
        actualLat must beCloseTo(lat, 0.005)
      }
    }
  }
}
