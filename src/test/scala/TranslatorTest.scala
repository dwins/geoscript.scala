package org.geoserver.community.css

import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

class TranslatorTest extends JUnitSuite with MustMatchersForJUnit {
  import Translator.color

  def body(x: Translator.OGCExpression): String = {
    x match {
      case x: org.opengis.filter.expression.Literal =>
        x.getValue.toString.toLowerCase
      case _ => null
    }
  }

  @Test 
  def colorFormats = {
    body(color(Literal("#FFAA77"))) must be ("#ffaa77")
    body(color(Function("rgb", List("1.0", "0.665", "0.467") map Literal))) must be ("#ffaa77")
    body(color(Literal("#FA7"))) must be ("#ffaa77")
    body(color(Function("rgb", List("255", "170", "119") map Literal))) must be ("#ffaa77")
  }
}

