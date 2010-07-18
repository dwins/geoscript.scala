package org.geoscript.geocss

import collection.JavaConversions._

import org.opengis.filter.{ PropertyIsEqualTo, Or }
import org.opengis.filter.expression.PropertyName

import org.specs._
/**
 * Tests for specific issues (mostly bugs that came up during testing)
 */
class Regressions extends Specification {
  def in(s: String) = getClass.getResourceAsStream(s)

  "overlapping scales should produce a single featuretypestyle" in {
    val stylesheet = CssParser.parse(in("/scales.css")).get
    val sld = Translator.css2sld(stylesheet)
    sld.featureTypeStyles must haveSize(1)
    sld.featureTypeStyles.get(0).rules must haveSize(2)
  }

  "Rules with conflicting filters cancel out" in {
    val styleSheet = CssParser.parse(in("/exclusive.css")).get
    val sld = Translator.css2sld(styleSheet)
    sld.featureTypeStyles must haveSize(1)
    sld.featureTypeStyles.get(0).rules must haveSize(9)
  }

  "Ratios should be expressible as decimals or percentages" in {
    val styleSheet = CssParser.parse(in("/percentage.css")).get
    val rule = styleSheet(0).asInstanceOf[Rule]
    rule.properties.find(_.name == "fill-opacity").map(_.values.head) must 
      beSome(List(Literal("50%")))
    rule.properties.find(_.name == "stroke-opacity").map(_.values.head) must
      beSome(List(Literal("0.50")))
  }

  "Colors should be accepted by the parser" in {
    val rules: List[Rule]= CssParser.parse(in("/states.css")).get
    val colorValue = rules(1).properties(0).values(0)
    colorValue.length must be (1)
    colorValue(0) must_== (Literal("#4DFF4D"))
  }

  "Using multiple typenames should produce multiple FeatureTypeStyles" in {
    val styleSheet = CssParser.parse(in("/typenames.css")).get
    val sld = Translator.css2sld(styleSheet)
    sld.featureTypeStyles.size must_== (3)
    val names = 
      for (ft <- sld.featureTypeStyles) yield {
        ft.featureTypeNames.headOption map { _.getLocalPart }
      }
    names must haveTheSameElementsAs(Seq(None, Some("states"), Some("cities")))
  }

  "The geometry expression should appear in the generated SLD" in {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val style = Translator.css2sld(styleSheet)
    val rules = style.featureTypeStyles.get(0).rules()
    for (
      r <- (0 until rules.size()) map (rules.get);
      sym <- (0 until r.symbolizers().size()) map (r.symbolizers.get)  
    ) {
      sym.getGeometry() must notBeNull
      sym.getGeometry() must haveSuperClass[PropertyName]
    }
  }

  "The parser should distinguish expressions from literals" in {
    val styleSheet = CssParser.parse(in("/states.css")).get
    val rules = styleSheet 

    val strokeWidth = rules(0).properties(3)
    strokeWidth.name must_== ("stroke-width")
    strokeWidth.values(0)(0) must_== (Expression("PERSONS/1000000"))
  }

  "Hatched strokes should be passed through" in {
    val styleSheet = CssParser.parse(in("/railroad.css")).get
    val style = Translator.css2sld(styleSheet)

    style.featureTypeStyles must haveSize(1)

    val ftStyle = style.featureTypeStyles.get(0)

    ftStyle.rules must haveSize(2)
    ftStyle.rules.find( _.getFilter.isInstanceOf[PropertyIsEqualTo] ) must
      beSomething
      
    ftStyle.rules.find(_.getFilter.isInstanceOf[Or]) must beSomething
    val rule = ftStyle.rules.find(_.getFilter.isInstanceOf[Or]).get
    val sym = rule.symbolizers.get(0)
    sym must haveSuperClass[org.geotools.styling.LineSymbolizer]
    val lineSym = sym.asInstanceOf[org.geotools.styling.LineSymbolizer]
    lineSym must notBeNull
    lineSym.getStroke() must notBeNull
    lineSym.getStroke().getGraphicStroke() must notBeNull
    lineSym.getStroke().getGraphicStroke().graphicalSymbols must notBeNull
    val mark = lineSym.getStroke.getGraphicStroke.graphicalSymbols.get(0)
    mark must haveSuperClass[org.geotools.styling.Mark]
    mark.asInstanceOf[org.geotools.styling.Mark]
       .getWellKnownName.evaluate(null) must_== "hatch"
  }
}
