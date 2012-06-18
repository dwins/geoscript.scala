package org.geoscript
package geometry

import com.vividsolutions.jts.geom.util.{
  AffineTransformation, NoninvertibleTransformationException
}

class RichTransform(tx: Transform) {
  def translated(dx: Double, dy: Double): Transform =
    new AffineTransformation(tx).translate(dx, dy)

  def sheared(x: Double, y: Double): Transform =
    new AffineTransformation(tx).shear(x, y)

  def scaled(x: Double, y: Double): Transform =
    new AffineTransformation(tx).scale(x, y)

  def rotated(theta: Double, aboutX: Double = 0, aboutY: Double = 0): Transform =
    new AffineTransformation(tx).rotate(theta, aboutX, aboutY)

  def reflected(x0: Double, y0: Double, x1: Double, y1: Double): Transform =
    new AffineTransformation(tx).reflect(x0, y0, x1, y1)

  def inverse: Option[Transform] =
    try {
      Some(tx.getInverse)
    } catch {
      case (_: NoninvertibleTransformationException) => None
    }

  def apply[G <: Geometry](g: G): G = tx.transform(g).asInstanceOf[G]
}
