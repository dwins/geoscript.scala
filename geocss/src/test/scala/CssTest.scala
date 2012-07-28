package org.geoscript.geocss

import scala.util.parsing.input.StreamReader
import org.specs2._
import org.{ geotools => gt }

/**
 * Generic tests to be applied in bulk to many styles
 */
class SmokeTest extends Specification { def is =
    basicExamples ^ end ^
    transformExamples ^ end

  val testFiles = Seq(
    "/test-basic.css"    -> 2,
    "/states.css"        -> 4,
    "/railroad.css"      -> 2,
    "/minimal.css"       -> 1,
    "/comprehensive.css" -> 1,
    "/scales.css"        -> 3,
    "/marks.css"         -> 2,
    "/gt-opts.css"       -> 1,
    "/default_point.css" -> 2,
    "/hospital.css"      -> 3)

  val basicExamples: specification.Fragments = 
    testFiles.foldLeft("Apply parser to some known-good inputs": specification.Fragments) {
      (fragments, example) => 
        val (path, expectedRules) = example
        fragments ^ path ! tryParsing(path, expectedRules)
    }

  val transformExamples: specification.Fragments =
    testFiles.foldLeft("Full transformation should always produce non-empty output": specification.Fragments) {
      (fragments, example) =>
        val (path, _) = example
        fragments ^ path ! tryTransforming(path)
    }

  def tryParsing(path: String, expectedRuleCount: Int) = {
    val source = Option(getClass.getResourceAsStream(path))
    val rules =
      for (stream <- source) yield CssParser.parse(stream)
    rules must beSome.which { res =>
      res.successful == true && res.get.size == expectedRuleCount
    }
  }

  def tryTransforming(path: String) = {
    val source = Option(getClass.getResourceAsStream(path))
    val rules = for (stream <- source) yield CssParser.parse(stream)
    rules must beSome.which { res =>
      res.successful == true && sldBytes(res.get).nonEmpty
    }
  }

  def sldBytes(rules: Seq[Rule]) = {
    val Translator = new Translator
    val sld = Translator.css2sld(rules)
    val tx = new gt.styling.SLDTransformer
    val bytes = new java.io.ByteArrayOutputStream
    tx.transform(sld, bytes)
    bytes.toByteArray
  }
}

/**
 * Tests of specific handling of the CSS AST
 */
class CssTest extends org.scalatest.FunSuite
with org.scalatest.matchers.ShouldMatchers {
  import CssOps.{ Specificity, expand }

  def expr(x: String) = 
    Selector.asSelector(gt.filter.text.ecql.ECQL.toFilter(x))

  val any = Accept
  val id = Id("states.9")
  val cql = expr("STATE_NAME LIKE '%ia'")

  val propLists = List(
    Property("stroke", List(List(Literal("red")), List(Literal("green")))),
    Property("opacity", List(List(Literal("0.70")))),
    Property("width", List(List(Literal("10")), List(Literal("8")), List(Literal("6"))))
  )

  // def is =
  test("Specificity") {
    Specificity(id)  should be > (Specificity(cql))
    Specificity(cql) should be > (Specificity(any))
    Specificity(id)  should be > (Specificity(any))
  }

  test("Property expansion (sizes)") {
    expand(propLists, "opacity") should have size(1)
    expand(propLists, "width") should have size(3)
    expand(propLists, "fill") should have size(0)
    expand(propLists, "stroke") should have size(2)
  }

  test("Property expansion (values)") {
    val expanded = expand(propLists, "stroke")
    expanded(0) should equal (Map(
      ("stroke" -> List(Literal("red"))),
      ("opacity" -> List(Literal("0.70"))),
      ("width" -> List(Literal("10")))))
    expanded(1) should equal (Map(
      ("stroke"  -> List(Literal("green"))),
      ("opacity" -> List(Literal("0.70"))),
      ("width"   -> List(Literal("8")))))
  }
}
