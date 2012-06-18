package org.geoscript
package projection

import geometry._
import org.geotools.geometry.jts.JTS

trait Projectable[T] {
  def project(from: Projection, to: Projection)(t: T): T
}

object Projectable {
  implicit def geometriesAreProjectable[T <: Geometry]:
    Projectable[T] = new Projectable[T] {
      def project(from: Projection, to: Projection)(t: T): T =
        JTS.transform(t, transform(from, to)).asInstanceOf[T]
    }

  implicit def envelopesAreProjectable: Projectable[Envelope] = 
    new Projectable[Envelope] {
      def project(from: Projection, to: Projection)(e: Envelope) = 
         JTS.transform(e, transform(from, to))
    }
}
