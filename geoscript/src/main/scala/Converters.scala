package org
package object geoscript {
  import geometry._

  implicit def enrichGeometry(g: Geometry): RichGeometry =
    new RichGeometry(g)

  implicit def enrichPoint(p: Point): RichPoint =
    new RichPoint(p)

  implicit def enrichEnvelope(e: Envelope) = 
    new RichEnvelope(e)
}
