package tutorial

import org.geoscript._, geometry._, render._

object GeometryBuffer extends App {
  val poly = point(0, 0).buffer(1)
  draw(Content(poly), canvas = new Window)
}

object GeometrySimplify extends App {
  val poly = point(0, 0).buffer(1)
  draw(Content(simplify(poly, 0.05)), canvas = new Window)
  draw(Content(simplify(poly, 0.1)), canvas = new Window)
}

object GeometryTransform extends App {
  val circle = point(0, 0).buffer(1)
  draw(
    Content(Seq(circle, Transform.translated(dx=0.75, dy=0)(circle))),
    canvas = new Window)

  val box = polygon(Seq((0, 0), (1, 0), (1, 1), (0, 1), (0, 0)))
  draw(
    Content(Seq(
      box, Transform.sheared(x=1, y=0).scaled(x=2, y=2)(box))),
      canvas = new Window)

  val bar = polygon(Seq((-5,-2),(5,-2),(5,2),(-5,2), (-5,-2)))
  val cross = bar union Transform.rotated(theta=math.toRadians(90))(bar)
  draw(
    Content(Seq(cross, Transform.rotated(theta=math.toRadians(45))(cross))),
    canvas = new Window)
}
