package org.geoscript.support.interval

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._, prop._

class IntervalCheck extends PropSpec with Checkers {
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

      Gen.oneOf[Interval[String]](lefts, rights, finite, Interval.Empty[String], Interval.Full[String])
    }

  property("join(x,y) always produces either x or y") {
    check { (x: Cap[String], y: Cap[String]) => 
      val joined = joinLeft(x, y)
      joined == x || joined == y
    }
  }
  
  property("Intersection with an empty interval produces an empty interval") {
    check { (x: Interval[String]) =>
      intersection(x, Interval.Empty) == Interval.Empty 
    }
  }

  property("Intersection with full") {
    check { (x: Interval[String]) => intersection(x, Interval.Full[String]) == x }
  }

  property("Intersection with self") {
    check { (x: Interval[String]) => intersection(x, x) == x }
  }

  property("Intersection is commutative") {
    check { (x: Interval[String], y: Interval[String]) => intersection(x, y) == intersection(y, x) }
  }
}
