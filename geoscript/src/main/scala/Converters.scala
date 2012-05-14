package org
package object geoscript {
  import geometry._
  import layer._

  implicit def enrichGeometry(g: Geometry): RichGeometry =
    new RichGeometry(g)

  implicit def enrichPoint(p: Point): RichPoint =
    new RichPoint(p)

  implicit def enrichEnvelope(e: Envelope) = 
    new RichEnvelope(e)

  implicit def enrichLayer(layer: Layer): RichLayer =
    new RichLayer(layer)
}
