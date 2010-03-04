package org.geoserver.community.css

import scala.util.parsing.input.StreamReader
import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

/**
 * General-purpose, one-size-fits-all tests for MSS parsing operations
 */
class CssTest extends CssOps with JUnitSuite with MustMatchersForJUnit {
  private def testFiles = List(
    ("/test-basic.css", 2),
    ("/states.css", 4),
    ("/railroad.css", 2),
    ("/minimal.css", 1),
    ("/comprehensive.css", 1),
    ("/scales.css", 3),
    ("/marks.css", 2),
    ("/gt-opts.css", 1)
  ) map {
    case (file, count) => (file, getClass.getResourceAsStream(file), count)
  }

  @Test def fileExists = testFiles foreach { _._2 must not be (null) }

  @Test def grammar = {
    for ((_, stream, _) <- testFiles) {
      val reader = new java.io.InputStreamReader(stream)
      val parseResult = CssParser.styleSheet(StreamReader(reader))
      parseResult.successful must be (true)
    }
  }

  @Test def rulesFound = {
    for ((file, stream, count) <- testFiles) {
      val styleSheet = CssParser.parse(stream)
      if (!styleSheet.successful) fail(file + "\n" + styleSheet.toString)
      if (styleSheet.get.length != count) println(styleSheet)
      styleSheet.get.length must be (count)
    }
  }

  @Test def SLDOutput = {
    val in = testFiles(5)._2
    val styleSheet = CssParser.parse(in).get
    val style = Translator.css2sld(styleSheet)
    val xform = new org.geotools.styling.SLDTransformer
    xform.setIndentation(2)
    xform.transform(style, new java.io.ByteArrayOutputStream)
  }

  @Test def specificity = {
    val any = AcceptSelector
    val id  = IdSelector("states.9")
    val cql = ExpressionSelector("STATE_NAME LIKE '%ia'")

    Specificity(id) must be > (Specificity(cql))
    Specificity(cql) must be > (Specificity(any))
    Specificity(id) must be > (Specificity(any))
  }
}
