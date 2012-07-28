package org.geoscript.geocss

import org.geoscript.geocss.testing._

import collection.JavaConversions._

import org.geotools.{ styling => gt }
import org.opengis.{ filter => ogc }

import org.scalatest.FunSuite
import org.scalatest.matchers.{
  ShouldMatchers, HavePropertyMatcher, HavePropertyMatchResult, Matcher }

/**
 * Tests for specific issues (mostly bugs that came up during testing)
 */
class Regressions extends FunSuite with ShouldMatchers {
  val Translator = new Translator

  def in(s: String) = getClass.getResourceAsStream(s)

  def property(name: String, values: List[List[Value]]): HavePropertyMatcher[Rule, String] =
    new HavePropertyMatcher[Rule, String] {
      def apply(r: Rule): HavePropertyMatchResult[String] = 
        new HavePropertyMatchResult(
          r.properties.exists(p => p.name == name && p.values == values),
          "property named",
          name,
          "<none>")
    }

  test("Overlapping scales should produce a single FeatureTypeStyle") {
    val stylesheet = CssParser.parse(in("/scales.css"))
    stylesheet should be ('successful)
    val sld = Translator.css2sld(stylesheet.get)
    sld.featureTypeStyles should have size(1)
    sld.featureTypeStyles.map(_.rules.size).sum should be (2)
  }

  test("Rules with conflicting filters cancel out") {
    val stylesheet = CssParser.parse(in("/exclusive.css"))
    stylesheet should be ('successful)
    val sld = Translator.css2sld(stylesheet.get)
    sld.featureTypeStyles should have size(1)
    sld.featureTypeStyles.map(_.rules.size).sum should be (9)
  }

  test("Overlapping scales should not hide filters") {
    val stylesheet = CssParser.parse(in("/motorvag.css"))
    stylesheet should be ('successful)
    val sld = Translator.css2sld(stylesheet.get)
    sld.featureTypeStyles should have size(1)
    val rules = sld.featureTypeStyles.flatMap(_.rules)
    rules should have size(2)
    val filters = rules.map(_.getFilter)
    filters should not(contain(ogc.Filter.INCLUDE: ogc.Filter))
  }

  test("Ratios should be expressible as decimals or percentages") {
    val stylesheet = CssParser.parse(in("/percentage.css"))
    stylesheet should be ('successful)
    val rule = stylesheet.get.head
    rule should have(property("fill-opacity", List(List(Literal("50%")))))
    rule should have(property("stroke-opacity", List(List(Literal("0.50")))))
  }

  test("Colors should be accepted by the parser") {
    val stylesheet = CssParser.parse(in("/states.css"))
    stylesheet should be ('successful)
    val rules = stylesheet.get
    rules(1) should have(property("fill", List(List(Literal("#4DFF4D")))))
  }

  test("Using multiple typenames should produce multiple FeatureTypeStyles") {
    val stylesheet = CssParser.parse(in("/typenames.css"))
    stylesheet should be ('successful)
    val sld = Translator.css2sld(stylesheet.get)
    val names = for (ft <- sld.featureTypeStyles) yield
                  for (name <- ft.featureTypeNames.headOption) yield name.getLocalPart
    names should containAll(None, Some("states"), Some("cities"))
  }

  test("The geometry expression should appear in the generated SLD") {
    val stylesheet = CssParser.parse(in("/states.css"))
    stylesheet should be ('successful)
    val sld = Translator.css2sld(stylesheet.get)
    val symbolizerGeometries = 
      for {
        ftStyle <- sld.featureTypeStyles
        rule <- ftStyle.rules
        symbolizer <- rule.symbolizers
      } yield symbolizer.getGeometry

    for (g <- symbolizerGeometries) 
      assert(g.isInstanceOf[ogc.expression.PropertyName])
  }

  test("The parser should distinguish expressions from literals") {
    val stylesheet = CssParser.parse(in("/states.css"))
    stylesheet should be ('successful)
    val rules = stylesheet.get
    rules.head should have(property("stroke-width", List(List(Literal("3")))))
  }

  test("Hatched strokes should be passed through") {
    val stylesheet = CssParser.parse(in("/railroad.css"))
    stylesheet should be ('successful)
    val style = Translator.css2sld(stylesheet.get)

    style.featureTypeStyles should have size(1)
  
    val allRules = style.featureTypeStyles.flatMap(_.rules)
    allRules should have size(2)

    val allFilters = allRules.map(_.getFilter)
    assert(allFilters.find(_.isInstanceOf[ogc.PropertyIsEqualTo]).isDefined)

    val ruleWithTheNotFilter = allRules.find(_.getFilter.isInstanceOf[ogc.Not])
    assert(ruleWithTheNotFilter.isDefined)
    assert(ruleWithTheNotFilter.get.symbolizers.head.isInstanceOf[gt.LineSymbolizer])

    val lineSym = ruleWithTheNotFilter.get.symbolizers.head.asInstanceOf[gt.LineSymbolizer]
    val graphicalSymbols =
      for {
        stroke <- Option(lineSym.getStroke)
        graphic <- Option(stroke.getGraphicStroke)
        symbols <- Option(graphic.graphicalSymbols)
        mark <- symbols.headOption.collect { case (m: gt.Mark) => m }
      } yield mark.getWellKnownName.evaluate(null)

    graphicalSymbols should be (Some("hatch"))
  }

  test("Conflicting scale limits should be placed in separate rules") {
    val stylesheet = CssParser.parse(in("/complex-scales.css"))
    stylesheet should be ('successful)
    val sld = Translator.css2sld(stylesheet.get)
    sld.featureTypeStyles should have size(1)
    sld.featureTypeStyles.flatMap(_.rules) should have size(2)
  }
}
