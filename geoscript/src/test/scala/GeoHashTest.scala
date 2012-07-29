package org.geoscript

import org.scalatest._, matchers._
import GeoHash._

class GeoHashTest extends FunSuite with ShouldMatchers {
  val cases = Seq(
    (57.64911, 10.40744, 11, "u4pruydqqvj"),
    (42.6, -5.6, 5, "ezs42")
  )

  test("produce the cited hashes") {
    cases.foreach { case (lon, lat, level, hash) => 
      geohash(lon, lat, level) should be(hash)
    }
  }

  test("work in reverse") {
    cases.foreach { case (lon, lat, level, hash) => 
      val (actualLon, actualLat) = decode(hash)
      assert(math.abs(actualLon - lon) < 0.005,
        "Actual longitude %f not within tolerance of expected %f" format(actualLon, lon))
      assert(math.abs(actualLat - lat) < 0.005,
        "Actual latitude %f not within tolerance of expected %f" format(actualLat, lat))
    }
  }
}
