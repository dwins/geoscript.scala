package org.geoscript.geocss.filter

import org.geoscript.support.interval.Interval
import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL.{ toFilter => cql }
import org.specs2._

class FiltersAreSententialTest extends Specification {
  import FiltersAreSentential._

  def is =
      "Constraints" ^
      "Null" ! {
        constraint(cql("A IS NULL")) must_== IsNull("A")
      } ^
      "Not Null" ! {
        constraint(cql("A IS NOT NULL")) must_== In("A", Interval.Full)
      } ^
      "Not equals" ! {
        constraint(cql("A <> 1")) must_== IsNot("A", Value("1"))
      } ^
      // "Disproves" ! {
      //   disprovenBy(Set(cql("A IS NULL")), cql("A <> 1")) must_== true
      // } ^
      "Disproves again" ! {
        disprovenBy(Set(cql("A <> 1")), cql("A IS NULL")) must_== true
      }
}
