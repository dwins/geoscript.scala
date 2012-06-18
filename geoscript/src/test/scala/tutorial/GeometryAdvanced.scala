package tutorial

import org.geoscript._, geometry._, render._

object GeometryBuffer extends App {
  val poly = point(0, 0).buffer(1)
  drawFull(Content(poly), new Window)
}

object GeometrySimplify extends App {
  val poly = point(0, 0).buffer(1)
  drawFull(Content(simplify(poly, 0.05)), new Window)
  drawFull(Content(simplify(poly, 0.1)), new Window)
}

object GeometryTransform extends App {
  val circle = point(0, 0).buffer(1)
  drawFull(
    Content(Seq(circle, Transform.translated(dx=0.75, dy=0)(circle))),
    new Window)

  val box = polygon(Seq((0, 0), (1, 0), (1, 1), (0, 1), (0, 0)))
  drawFull(
    Content(Seq(
      box, Transform.sheared(x=1, y=0).scaled(x=2, y=2)(box))),
    new Window)

  val bar = polygon(Seq((-5,-2),(5,-2),(5,2),(-5,2), (-5,-2)))
  val cross = bar union Transform.rotated(theta=math.toRadians(90))(bar)
  drawFull(
    Content(Seq(cross, Transform.rotated(theta=math.toRadians(45))(cross))),
    new Window)
}
