package org { 
  package object geoscript {
    import geometry._

    implicit def enrichGeometry(geometry: Geometry): RichGeometry =
      new RichGeometry(geometry)

    implicit def enrichEnvelope(envelope: Envelope): RichEnvelope =
      new RichEnvelope(envelope)

    implicit def enrichPoint(point: Point): RichPoint =
      new RichPoint(point)

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
}
