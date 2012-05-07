package org
package object geoscript {
  import geometry._

  implicit def enrichGeometry(g: Geometry): RichGeometry =
    new RichGeometry(g)

  implicit def enrichPoint(p: Point): RichPoint =
    new RichPoint(p)

  implicit def enrichEnvelope(e: Envelope) = 
    new RichEnvelope(e)

  implicit def pointFromPairOfCoordinates[N : Numeric](
    tuple: (N, N)
  ): Point = {
    val ops = implicitly[Numeric[N]]
    Point(ops.toDouble(tuple._1), ops.toDouble(tuple._2))
  }

  implicit def pointFromTripleOfCoordinates[N : Numeric](
    tuple: (N, N, N)
  ): Point = {
    val ops = implicitly[Numeric[N]]
    Point(
      ops.toDouble(tuple._1),
      ops.toDouble(tuple._2),
      ops.toDouble(tuple._3)
    )
  }
}
