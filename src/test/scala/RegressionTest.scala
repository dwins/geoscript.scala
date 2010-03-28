package org.geoserver.community.css

import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

/**
 * Tests for specific issues (mostly bugs that came up during testing)
 */
class RegressionTest extends JUnitSuite with MustMatchersForJUnit with TypeMatcher {
  def in(s: String) = getClass.getResourceAsStream(s)

  private def extractColor(v: Value) = {
    v match {
      case l: Literal => l.body
      case _ => null
    }
  }

  @Test def lotsOfExclusiveFilters = {
    val styleSheet = CssParser.parse(in("/exclusive.css")).get
    val sld = Translator.css2sld(styleSheet)
    sld.featureTypeStyles.size must be (2)
    sld.featureTypeStyles.get(0).rules must have (size(9))
    sld.featureTypeStyles.get(1).rules must have (size(9))
  }

  @Test def percentage = {
    val styleSheet = CssParser.parse(in("/percentage.css")).get
    val rule = styleSheet(0).asInstanceOf[Rule]
    val fillOpacity =
      rule.properties.find(x => x.name == "fill-opacity").get
    val strokeOpacity =
      rule.properties.find(x => x.name == "stroke-opacity").get
    fillOpacity.values.first must be (List(Literal("50%")))
    strokeOpacity.values.first must be (List(Literal("0.50")))
  }

  @Test def malformedCQL = {
    CssParser.parse(in("/badstyle.css")) must have (
      parent (classOf[CssParser.Failure])
    )
  }

  @Test def color = {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val rules = styleSheet filter {
      (x: Product) => x.isInstanceOf[Rule]
    } map {
      _.asInstanceOf[Rule]
    }
    val colorValue = rules(1).properties(1).values(0)
    colorValue.length must be (1)
    extractColor(colorValue(0)) must be ("#4DFF4D")
  }

  @Test def typenames = {
    val styleSheet = CssParser.parse(in("/typenames.css")).get
    val sld = Translator.css2sld(styleSheet)
    sld.featureTypeStyles.size must be (5)
  }

  @Test def geometry = {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val style = Translator.css2sld(styleSheet)
    val geom = style.featureTypeStyles.get(0)
      .rules.get(0)
      .symbolizers.get(0)
      .getGeometry
    geom must not be (null)
    geom must have (
      parent (classOf[org.opengis.filter.expression.PropertyName])
    )
  }

  @Test def typename = {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val style = Translator.css2sld(styleSheet)
    val it = style.featureTypeStyles.iterator
    while (it.hasNext) { it.next.getFeatureTypeName must be ("states") }
  }

  @Test def expression = {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val rules = styleSheet 

    val strokeWidth = rules(0).properties(3)
    strokeWidth.name must be ("stroke-width")
    strokeWidth.values(0)(0) must be (Expression("PERSONS/1000000"))
  }

  @Test def stroke = {
    val styleSheet = CssParser.parse(in("/railroad.css")).get
    val style = Translator.css2sld(styleSheet)
    val rule = style.featureTypeStyles.get(0).rules.get(1)
    rule.getFilter must have (parent (classOf[org.opengis.filter.Not]))
    val sym = rule.symbolizers.get(0)
    sym must have (parent (classOf[org.geotools.styling.LineSymbolizer]))
    val lineSym = sym.asInstanceOf[org.geotools.styling.LineSymbolizer]
    val mark = lineSym.getStroke.getGraphicStroke.graphicalSymbols.get(0)
    mark must have (parent (classOf[org.geotools.styling.Mark]))
    mark.asInstanceOf[org.geotools.styling.Mark]
       .getWellKnownName.evaluate(null) must be ("hatch")
  }
}
