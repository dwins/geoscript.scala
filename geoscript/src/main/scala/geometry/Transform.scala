package org.geoscript
package geometry

import com.vividsolutions.jts.geom.util.AffineTransformation

class Transform(tx: AffineTransformation) {
  def apply[G <: Geometry](g: G): G = {
    val under = g.underlying.clone().asInstanceOf[com.vividsolutions.jts.geom.Geometry]
    under.apply(tx)
    Geometry(under).asInstanceOf[G]
  }

  def translate(dx: Double, dy: Double): Transform =
    new Transform(new AffineTransformation(tx).translate(dx, dy))

  def shear(shx: Double, shy: Double): Transform =
    new Transform(new AffineTransformation(tx).shear(shx, shy))

  def scale(sx: Double, sy: Double): Transform =
    new Transform(new AffineTransformation(tx).scale(sx, sy))
}

object Transform extends Transform(new AffineTransformation) {
}
