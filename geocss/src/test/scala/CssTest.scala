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
class CssTest extends Specification { 
  import CssOps.{ Specificity, expand }

  def is =
    "Specificity comparison" ^
      "id > filter" ! {
        Specificity(id) must be_> (Specificity(cql))
      } ^
      "filter > (no selector)" ! {
        Specificity(cql) must be_> (Specificity(any))
      } ^ 
      "id > (no selector)" ! {
        Specificity(id) must be_> (Specificity(any))
      } ^ end ^
    "Property expansion" ^
      "have length 1 when 'key' property has length 1" ! {
        expand(propLists, "opacity") must haveSize(1)
      } ^ 
      "have length 3 when 'key' property has length 3" ! {
        expand(propLists, "width") must haveSize(3)
      } ^ 
      "have length 0 when 'key' property is undefined" ! {
        expand(propLists, "fill") must haveSize(0)
      } ^ end ^
    "Detailed check" ^ 
      "have length 2 when 'key' property has length 2" ! {
        expand(propLists, "stroke") must haveSize(2)
      } ^
      "expected values for first expansion" ! {
        expand(propLists, "stroke")(0) must havePairs(
          "stroke"  -> List(Literal("red")),
          "opacity" -> List(Literal("0.70")),
          "width"   -> List(Literal("10"))
        )
      } ^
      "expected values for second expansion" ! {
        expand(propLists, "stroke")(1) must havePairs(
          "stroke"  -> List(Literal("green")),
          "opacity" -> List(Literal("0.70")),
          "width"   -> List(Literal("8"))
        ) 
      } ^ end

  lazy val any = Accept
  lazy val id = Id("states.9")
  lazy val cql = expr("STATE_NAME LIKE '%ia'")

  val propLists = List(
    Property("stroke", List(List(Literal("red")), List(Literal("green")))),
    Property("opacity", List(List(Literal("0.70")))),
    Property("width", List(List(Literal("10")), List(Literal("8")), List(Literal("6"))))
  )

  import gt.filter.text.ecql.ECQL.toFilter
  lazy val expr = (toFilter(_: String)) andThen (Selector.asSelector)
}
