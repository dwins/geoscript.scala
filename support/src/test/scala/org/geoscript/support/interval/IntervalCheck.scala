package org.geoscript.support.interval

import org.scalacheck._, Arbitrary.arbitrary, Prop._

object IntervalCheck extends Properties("Intervals") {
  import Interval.intersection
  val joinLeft = Cap.join[String](_ < _) _

  implicit val arbCap: Arbitrary[Cap[String]] = 
    Arbitrary { 
      for {
        s <- Gen.identifier
        b <- Gen.oneOf(true, false)
      } yield if (b) Open(s) else Closed(s)
    }

  implicit val arbInterval: Arbitrary[Interval[String]] =
    Arbitrary {
      val lefts = arbitrary[Cap[String]].map(_.left)
      val rights = arbitrary[Cap[String]].map(_.right)
      val finite = 
        for { 
          a <- arbitrary[Cap[String]]
          b <- arbitrary[Cap[String]]
        } yield {
          if (a.v < b.v)
            Interval.finite(a, b)
          else
            Interval.finite(b, a)
        }

      Gen.oneOf(lefts, rights, finite, Interval.Empty[String], Interval.Full[String])
    }

  property("join(x,y) always produces either x or y") =
    forAll { (x: Cap[String], y: Cap[String]) => 
      val joined = joinLeft(x, y)
      joined == x || joined == y
    }
  
  property("Intersection with an empty interval produces an empty interval") =
    forAll { (x: Interval[String]) =>
      intersection(x, Interval.Empty) == Interval.Empty 
    }

  property("Intersection with full") =
    forAll { (x: Interval[String]) => intersection(x, Interval.Full[String]) == x }

  property("Intersection with self") =
    forAll { (x: Interval[String]) => intersection(x, x) == x }

  property("Intersection is commutative") =
    forAll { (x: Interval[String], y: Interval[String]) => intersection(x, y) == intersection(y, x) }
}
