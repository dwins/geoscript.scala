package org.geoscript.support.interval

import org.specs2._

class IntervalSpec extends Specification {
  def is = 
    "Intervals" ! {
      (Open(1).right contains 2).aka(Open(1).right.toString) must beFalse
    }
}
