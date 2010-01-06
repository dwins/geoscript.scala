package org.geoscala

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class GeoHashTest extends FlatSpec with GeoHash with ShouldMatchers{
  val cases = Map(
    (57.64911, 10.40744, 11) -> "u4pruydqqvj"
  )

  "hash" should "be reversible" in {
    val invertedCases = Map() ++ (cases.map(x => {
        (x._2, x._1)
      }))

    cases.keys.foreach( x => 
      geohash(x._1, x._2, x._3) should be (cases(x))
    )

    invertedCases.keys.foreach(x => {
        dehash(x)._1 should be (invertedCases(x)._1 plusOrMinus 0.0001)
        dehash(x)._2 should be (invertedCases(x)._2 plusOrMinus 0.0001)
      }
    )
  }
}
