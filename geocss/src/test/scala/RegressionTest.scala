package org.geoscript.geocss

import collection.JavaConversions._

import org.geotools.{ styling => gt }
import org.opengis.{ filter => ogc }

import org.specs2._
/**
 * Tests for specific issues (mostly bugs that came up during testing)
 */
class Regressions extends Specification {
  val Translator = new Translator
  def in(s: String) = getClass.getResourceAsStream(s)

  def is =
    "Overlapping scales should produce a single FeatureTypeStyle" ! {
      val stylesheet = CssParser.parse(in("/scales.css")).get
      val sld = Translator.css2sld(stylesheet)
      sld must (
        haveFeatureTypeStyleCount(1) and
        haveRuleCount(2)
      )
    } ^
    "Rules with conflicting filters cancel out" ! {
      val styleSheet = CssParser.parse(in("/exclusive.css")).get
      val sld = Translator.css2sld(styleSheet)
      sld must (
        haveFeatureTypeStyleCount(1)
       //  and
       // haveRuleCount(9)
      )
    } ^
    "overlapping scales should not hide filters" ! {
      val styleSheet = CssParser.parse(in("/motorvag.css")).get
      val sld = Translator.css2sld(styleSheet)
      sld must (
        haveFeatureTypeStyleCount(1) and
        haveRuleCount(2) and
        not(haveRuleWithFilter(ogc.Filter.INCLUDE))
      )
    } ^
    "Ratios should be expressible as decimals or percentages" ! {
      // TODO: Move this to the parser tests.
      val styleSheet = CssParser.parse(in("/percentage.css")).get
      val rule = styleSheet(0).asInstanceOf[Rule]
      rule must (
        haveProperty("fill-opacity").withValues(List(List(Literal("50%")))) and
        haveProperty("stroke-opacity").withValues(List(List(Literal("0.50"))))
      )
    } ^
    "Colors should be accepted by the parser" ! {
      val rules: Seq[Rule]= CssParser.parse(in("/states.css")).get
      val colorValue = rules(1).properties(0).values(0)
      rules(1) must (
        haveProperty("fill").withValues(List(List(Literal("#4DFF4D"))))
      )
    } ^
    "Using multiple typenames should produce multiple FeatureTypeStyles" ! {
      val styleSheet = CssParser.parse(in("/typenames.css")).get
      val sld = Translator.css2sld(styleSheet)
      val names = 
        for (ft <- sld.featureTypeStyles) yield
          ft.featureTypeNames.headOption map { _.getLocalPart }
      names must haveTheSameElementsAs(Seq(None, Some("states"), Some("cities")))
    } ^
    "The geometry expression should appear in the generated SLD" ! {
      val styleSheet = CssParser.parse(in("/states.css")).get
      val style = Translator.css2sld(styleSheet)
      val symbolizerGeometries = (s: gt.Style) =>
        for {
          ftStyle <- s.featureTypeStyles
          rule <- ftStyle.rules
          symbolizer <- rule.symbolizers
        } yield symbolizer.getGeometry

      style must(
        not(beNull[Any]) and
        beAnInstanceOf[ogc.expression.PropertyName]
      ).forall ^^ symbolizerGeometries
    } ^
    "The parser should distinguish expressions from literals" ! {
      val styleSheet = CssParser.parse(in("/states.css")).get
      val rules = styleSheet 

      rules.head must 
        haveProperty("stroke-width").withValues(List(List(Literal("3"))))
    } ^
    "Hatched strokes should be passed through" ! {
      val styleSheet = CssParser.parse(in("/railroad.css")).get
      val style = Translator.css2sld(styleSheet)

      val allFilters = allRules andThen (_ map (_.getFilter))
      val firstSymbolizer = (_: gt.Rule).symbolizers.head
      val symbolizerForTheOrFilter = 
        allRules andThen (
          _.find(_.getFilter.isInstanceOf[ogc.Not])
           .map(firstSymbolizer)
           .get
        )
      val lineSymbolizerForTheOrFilter =
        symbolizerForTheOrFilter andThen (_.asInstanceOf[gt.LineSymbolizer])

      style must (
        (haveSize[Seq[gt.FeatureTypeStyle]](1) ^^ featureTypeStyles) and
        (haveSize[Seq[gt.Rule]](2) ^^ allRules) and
        (beAnInstanceOf[ogc.PropertyIsEqualTo].atLeastOnce ^^ allFilters) and
        (beAnInstanceOf[gt.LineSymbolizer] ^^ symbolizerForTheOrFilter) and
        (haveGraphicStroke("hatch") ^^ lineSymbolizerForTheOrFilter)
      )
    } ^
    "Conflicting scale limits should be placed in separate rules" ! {
      val stylesheet = CssParser.parse(in("/complex-scales.css")).get
      val sld = Translator.css2sld(stylesheet)
      sld must (
        (haveSize[Seq[gt.FeatureTypeStyle]](1) ^^ featureTypeStyles) and
        (haveSize[Seq[gt.Rule]](2) ^^ allRules)
      )
    }

  val featureTypeStyles : gt.Style => Seq[gt.FeatureTypeStyle] = 
    _.featureTypeStyles.toList

  val allRules = (_: gt.Style).featureTypeStyles.flatMap(_.rules)

  def haveFeatureTypeStyleCount(n: Int): matcher.Matcher[gt.Style] =
    haveSize[Seq[gt.FeatureTypeStyle]](n) ^^ (
      (_: gt.Style).featureTypeStyles.toSeq
    )

  def haveRuleCount(n: Int): matcher.Matcher[gt.Style] =
    haveSize[Seq[gt.Rule]](n) ^^ (
      (_: gt.Style).featureTypeStyles.flatMap(_.rules)
    )

  def haveRuleWithFilter(f: ogc.Filter): matcher.Matcher[gt.Style] =
    new matcher.Matcher[gt.Style] {
      override def apply[S <: gt.Style](exp: matcher.Expectable[S])
      : matcher.MatchResult[S] = {
        result(
          exp.value.featureTypeStyles.exists { 
            ft => ft.rules.exists(_.getFilter == f)
          },
          "%s has rule with filter %s" format(exp.description, f),
          "%s has no rules with filter %s" format (exp.description, f),
          exp
        )
      }
    }

  def haveGraphicStroke(markName: String): matcher.Matcher[gt.LineSymbolizer] = {
    val graphicalSymbols = (sym: gt.LineSymbolizer) =>
      for {
        stroke <- Option(sym.getStroke)
        graphic <- Option(stroke.getGraphicStroke)
        symbols <- Option(graphic.graphicalSymbols)
        mark <- symbols.headOption.collect { case (m: gt.Mark) => m }
      } yield mark.getWellKnownName.evaluate(null)
    beEqualTo(Some(markName)) ^^ graphicalSymbols
  }
  
  case class haveProperty(name: String) extends matcher.Matcher[Rule] {
    def withValues(vss: List[List[Value]]): matcher.Matcher[Rule] = 
      new matcher.Matcher[Rule] {
        override def apply[R <: Rule](exp: matcher.Expectable[R])
        : matcher.MatchResult[R] = {
          val found = 
            exp.value.properties.exists(p => p.name == name && p.values == vss)
          result(
            found,
            "%s has '%s' property with values %s" format(exp.description, name, vss),
            "%s has no '%s' property with values %s" format (exp.description, name, vss),
            exp
          )
        }
      }

    override def apply[R <: Rule](exp: matcher.Expectable[R])
    : matcher.MatchResult[R] = {
      result(
        exp.value.properties.exists(_.name == name),
        "%s has '%s' property" format(exp.description, name),
        "%s has no '%s' property" format (exp.description, name),
        exp
      )
    }
  }
}
