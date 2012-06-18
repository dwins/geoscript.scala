package org.geoscript

package object viewer {
}

package viewer {
  trait Viewable[V] {
    def bounds(v: V): geometry.Envelope
    def draw(v: V, canvas: java.awt.Graphics2D): Unit
  }

  object Viewable {
    implicit object geometryIsViewable extends Viewable[geometry.Geometry] {
      def bounds(g: geometry.Geometry): geometry.Envelope =
        g.getEnvelopeInternal()

      def draw(g: geometry.Geometry, canvas: java.awt.Graphics2D) {
      }
    }
  }
}
