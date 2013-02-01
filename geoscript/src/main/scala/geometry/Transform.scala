package org.geoscript.geometry

import com.vividsolutions.jts.geom.util.AffineTransformation

class Transform(tx: AffineTransformation) {
  def apply[G <: Geometry](g: G): G = {
    val res = g.clone().asInstanceOf[G]
    res.apply(tx)
    res
  }

  def translate(dx: Double, dy: Double): Transform =
    new Transform(new AffineTransformation(tx).translate(dx, dy))

  def shear(shx: Double, shy: Double): Transform =
    new Transform(new AffineTransformation(tx).shear(shx, shy))

  def scale(sx: Double, sy: Double): Transform =
    new Transform(new AffineTransformation(tx).scale(sx, sy))
}

object Transform extends Transform(new AffineTransformation)
