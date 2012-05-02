package test.scala.tutorial
package tutorial

object StylingAndRendering {
  import org.geoscript.style.combinators._

  val style = Stroke("black", width=2) and Fill("#FF0000", opacity=0.75)
  // draw
}
