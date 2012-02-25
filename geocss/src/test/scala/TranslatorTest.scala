package org.geoscript.geocss

import org.specs2._

class TranslatorTest extends Specification with matcher.DataTables {
  val Translator = new Translator
  import Translator.color
  
  def body(x: Translator.OGCExpression): String = {
    x match {
      case x: org.opengis.filter.expression.Literal =>
        x.getValue.toString.toLowerCase
      case _ => null
    }
  }

  def is = 
    "Short hexcodes and English names should work as colors" ! {
      "color"            | "hex" |
      Literal("#FFAA77") ! "#ffaa77" |
      Function("rgb", List("1.0", "0.665", "0.467") map Literal) ! "#ffaa77" |
      Literal("#FA7")    ! "#ffaa77" |
      Function("rgb", List("255", "170", "119") map Literal) ! "#ffaa77" |> {
        (property, expected) => body(color(property)) must_== expected
      }
    }
}
