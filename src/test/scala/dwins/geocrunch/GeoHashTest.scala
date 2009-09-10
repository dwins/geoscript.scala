package dwins.geocrunch

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnit3Suite

class GeoHashTest extends JUnit3Suite with GeoHash with ShouldMatchers{
  val cases = Map(
    (57.64911, 10.40744, 11) -> "u4pruydqqvj"
  )

  def testHash() {
    val invertedCases = Map() incl (cases.map(x => {
        (x._2, x._1)
    }))

    cases.keys.foreach( x => {
        assert(geohash(x._1, x._2, x._3) == cases(x))
      }
    )

    invertedCases.keys.foreach(x => {
        dehash(x)._1 should be (invertedCases(x)._1 plusOrMinus 0.0001)
        dehash(x)._2 should be (invertedCases(x)._2 plusOrMinus 0.0001)
      }
    )
  }
}
