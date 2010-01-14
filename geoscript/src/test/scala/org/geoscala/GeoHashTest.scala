package org.geoscript

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class GeoHashTest extends FlatSpec with GeoHash with ShouldMatchers{
  val cases = Seq(
    (57.64911, 10.40744, 11, "u4pruydqqvj"),
    (42.6, -5.6, 5, "ezs42")
  )

  "hash" should "be reversible" in {
    cases.foreach { case (lon, lat, level, hash) => 
      geohash(lon, lat, level) should be (hash)
    }

    cases.foreach { case (lon, lat, level, hash) => 
      val (actualLon, actualLat) = decode(hash)
      actualLon should be (lon plusOrMinus 0.5)
      actualLat should be (lat plusOrMinus 0.5)
    }
  }
}
