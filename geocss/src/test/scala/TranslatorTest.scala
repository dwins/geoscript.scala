package org.geoscript.geocss

import org.specs._

class TranslatorTest extends Specification {
  val Translator = new Translator
  import Translator.color

  def body(x: Translator.OGCExpression): String = {
    x match {
      case x: org.opengis.filter.expression.Literal =>
        x.getValue.toString.toLowerCase
      case _ => null
    }
  }

  "Short hexcodes and English names should work as colors" in {
    body(color(Literal("#FFAA77"))) must_== ("#ffaa77")
    body(color(Function("rgb", List("1.0", "0.665", "0.467") map Literal))) must_== ("#ffaa77")
    body(color(Literal("#FA7"))) must_== ("#ffaa77")
    body(color(Function("rgb", List("255", "170", "119") map Literal))) must_== ("#ffaa77")
  }
}
