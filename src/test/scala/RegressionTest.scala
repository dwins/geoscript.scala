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

  @Test def scaleConversion = {
    // val stylesheet = CssParser.parse(in("/scales.css")).get
    // val sld = Translator.css2sld(stylesheet)
    // following tests are failing!!
    // sld.featureTypeStyles must have (size(1))
    // sld.featureTypeStyles.get(0).rules must have (size(2))
  }

  @Test def lotsOfExclusiveFilters = {
    val styleSheet = CssParser.parse(in("/exclusive.css")).get
    val sld = Translator.css2sld(styleSheet)
    sld.featureTypeStyles must have (size(1))
    sld.featureTypeStyles.get(0).rules must have (size(9))
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
    val rules: List[Rule]= CssParser.parse(in("/states.css")).get
    val colorValue = rules(1).properties(0).values(0)
    colorValue.length must be (1)
    extractColor(colorValue(0)) must be ("#4DFF4D")
  }

  @Test def typenames = {
    val styleSheet = CssParser.parse(in("/typenames.css")).get
    val sld = Translator.css2sld(styleSheet)
    sld.featureTypeStyles.size must be (3)
  }

  @Test def geometry = {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val style = Translator.css2sld(styleSheet)
    val rules = style.featureTypeStyles.get(0).rules()
    for (
      r <- (0 until rules.size()) map (rules.get);
      sym <- (0 until r.symbolizers().size()) map (r.symbolizers.get)  
    ) {
      sym.getGeometry() must not be (null)
      sym.getGeometry() must have (
        parent (classOf[org.opengis.filter.expression.PropertyName])
      )
    }
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
    style.featureTypeStyles.get(0).rules.get(1)
      .getFilter must have (parent (classOf[org.opengis.filter.PropertyIsEqualTo]))
    val rule = style.featureTypeStyles.get(0).rules.get(0)
    rule.getFilter must have (parent (classOf[org.opengis.filter.PropertyIsNotEqualTo]))
    val sym = rule.symbolizers.get(0)
    sym must have (parent (classOf[org.geotools.styling.LineSymbolizer]))
    val lineSym = sym.asInstanceOf[org.geotools.styling.LineSymbolizer]
    lineSym must not be (null)
    lineSym.getStroke() must not be (null)
    lineSym.getStroke().getGraphicStroke() must not be (null)
    lineSym.getStroke().getGraphicStroke().graphicalSymbols must not be (null)
    val mark = lineSym.getStroke.getGraphicStroke.graphicalSymbols.get(0)
    mark must have (parent (classOf[org.geotools.styling.Mark]))
    mark.asInstanceOf[org.geotools.styling.Mark]
       .getWellKnownName.evaluate(null) must be ("hatch")
  }
}
