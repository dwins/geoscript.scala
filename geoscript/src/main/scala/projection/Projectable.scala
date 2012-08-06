package org.geoscript
package projection

import geometry._
import org.geotools.geometry.jts.{ JTS, ReferencedEnvelope }

trait Projectable[T] {
  def extractProjection(t: T): Option[Projection]
  def lift(t: T): Option[Referenced[T]] = 
    extractProjection(t).map(Referenced(t, _)(this))
  def project(from: Projection, to: Projection)(t: T): T
}

object Projectable {
  implicit def geometriesAreProjectable[T <: Geometry]:
    Projectable[T] = new Projectable[T] {
      def extractProjection(t: T): Option[Projection] = 
        if (t.getSRID > 0)
          fromSrid("EPSG:" + t.getSRID)
        else
          None

      def project(from: Projection, to: Projection)(t: T): T =
        JTS.transform(t, transform(from, to)).asInstanceOf[T]
    }

  implicit def envelopesAreProjectable: Projectable[Envelope] = 
    new Projectable[Envelope] {
      def extractProjection(e: Envelope): Option[Projection] = None

      def project(from: Projection, to: Projection)(e: Envelope) = 
         JTS.transform(e, transform(from, to))
    }
}