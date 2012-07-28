package org.geoscript.geocss

import org.{ geotools => gt }
import org.scalatest.FunSuite, org.scalatest.matchers.ShouldMatchers

/**
 * Generic tests to be applied in bulk to many styles
 */
class SmokeTest extends FunSuite with ShouldMatchers { 
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

  def tryParsing(path: String, expectedRuleCount: Int) = {
    val source = getClass().getResourceAsStream(path)
    source should not be null
    val rules = CssParser.parse(source)
    rules should be ('successful)
    rules.get.size should equal (expectedRuleCount)
  }

  def tryTransforming(path: String) = {
    val source = getClass.getResourceAsStream(path)
    source should not be null
    val rules = CssParser.parse(source)
    rules should be ('successful)
    sldBytes(rules.get) should be ('nonEmpty)
  }

  def sldBytes(rules: Seq[Rule]): Seq[Byte] = {
    val Translator = new Translator
    val sld = Translator.css2sld(rules)
    val tx = new gt.styling.SLDTransformer
    val bytes = new java.io.ByteArrayOutputStream
    tx.transform(sld, bytes)
    bytes.toByteArray
  }

  test("Apply parser to sample stylesheets") {
    for ((path, expectedRuleCount) <- testFiles) 
      tryParsing(path, expectedRuleCount)
  }

  test("Generate SLD for sample stylesheets") {
    for ((path, _) <- testFiles) 
      tryTransforming(path)
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
