package org.geoscript.geocss

import math._
import util.Sorting.stableSort

import org.geoscript.support.logic.reduce

import org.geotools.feature.NameImpl
import org.geotools.{styling => gt}
import org.geotools.styling.{
  LineSymbolizer,
  PointSymbolizer,
  PolygonSymbolizer,
  Symbolizer,
  TextSymbolizer,
  TextSymbolizer2,
  FeatureTypeStyle
}

import org.opengis.{ filter => ogc }

/**
 * The Translator object houses some facilities for converting token lists to
 * GeoTools Style objects.  The css2sld method does the actual conversion.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
class Translator(val baseURL: Option[java.net.URL]) {
  def this() = this(None)
  def this(url: String) = this(Some(new java.net.URL(url)))

  import CssOps.{ Color, Specificity, Symbol, URL, colors, expand }
  val styles = org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)
  type OGCExpression = org.opengis.filter.expression.Expression

  val gtVendorOpts = Seq(
    "-gt-label-padding" -> "spaceAround",
    "-gt-label-group" -> "group",
    "-gt-label-max-displacement" -> "maxDisplacement",
    "-gt-label-min-group-distance" -> "minGroupDistance",
    "-gt-label-repeat" -> "repeat",
    "-gt-label-all-group" -> "allGroup",
    "-gt-label-remove-overlaps" -> "removeOverlaps",
    "-gt-label-allow-overruns" -> "allowOverrun",
    "-gt-label-follow-line" -> "followLine",
    "-gt-label-max-angle-delta" -> "maxAngleDelta",
    "-gt-label-auto-wrap" -> "autoWrap",
    "-gt-label-force-ltr" -> "forceLeftToRight",
    "-gt-label-conflict-resolution" -> "conflictResolution",
    "-gt-label-fit-goodness" -> "goodnessOfFit",
    "-gt-shield-resize" -> "graphic-resize",
    "-gt-shield-margin" -> "graphic-margin"
  )

  private val defaultRGB = filters.literal(colors("grey"))

  def resolve(path: String): String =
    new java.net.URL(baseURL.getOrElse(null), path).toString

// externalGraphic, well-known graphic , color
  def fill(xs: Seq[Value]): (String, String, OGCExpression) = {
    (xs take 2) match {
      case Seq(URL(url), Color(color)) =>
        (resolve(url), null, filters.literal(color))
      case Seq(URL(url)) =>
        (resolve(url), null, defaultRGB)
      case Seq(Symbol(sym)) =>
        (null, sym, defaultRGB)
      case Seq(Color(color))  =>
        (null, null, filters.literal(color))
      case _ =>
        (null, null, defaultRGB)
    }
  }

  def buildGraphic(
    prefix: String,
    props: Map[String, Seq[Value]],
    markProps: Seq[Property]
  ): Option[gt.Graphic] = {
    def p(name: String) = 
      props.get(prefix + "-" + name) orElse
      markProps.find(_.name == name).flatMap(_.values.headOption)

    val (url, wellKnownName, _) = fill(props(prefix))
    val mimetype = p("mime") map keyword 
    val size = p("size") map length
    val rotation = p("rotation") map angle
    val opacity = p("opacity") map scale getOrElse null

    val mark = buildMark(
      wellKnownName,
      size,
      rotation getOrElse filters.literal(0),
      markProps
    )

    val externalGraphic = buildExternalGraphic(url, mimetype)

    if (mark != null || externalGraphic != null)
      Some(styles.createGraphic(
        externalGraphic,
        mark,
        null,
        opacity,
        size getOrElse null,
        rotation getOrElse null
      ))
    else
      None
  }

  def buildMark(
    markName: String, 
    width: Option[OGCExpression], 
    rotation: OGCExpression,
    markProps: Seq[Property]
  ): Array[gt.Mark] = {
    if (markName != null) {
      val strokeAndFill = (
        expand(markProps, "stroke").headOption.map(extractStroke(_, Nil)),
        expand(markProps, "fill").headOption.map(extractFill(_, Nil))
      )

      val (stroke, fill) = strokeAndFill match {
        case (None, None) =>
          (styles.getDefaultStroke(), styles.getDefaultFill())
        case (s, f) => (s.getOrElse(null), f.getOrElse(null))
      }

      Array(styles.createMark(
        filters.literal(markName),
        stroke,
        fill,
        filters.literal(16),
        filters.literal(0)
      ))
    } else {
      null
    }
  }

  def buildExternalGraphic(url: String, mimetype: Option[String]) = {
    if (url != null) {
      Array(styles.createExternalGraphic(url, mimetype.getOrElse("image/jpeg")))
    } else null
  }

  def color(v: Value) = v match {
    case Color(c) => filters.literal(c)
    case _ => defaultRGB
  }

  // GeoTools only knows about #RRGGBB, but CSS lets you use nice names as well...
  def color(p: Property) = p.values.head.head match {
    case Literal(body) => filters.literal(colors.getOrElse(body, body))
    case _ => null
  }

  def angle(xs: Seq[Value]) = xs.head match {
    case l: Literal =>
      filters.literal(l.body.replaceFirst("deg$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def length(xs: Seq[Value]): OGCExpression = xs.head match {
    case Literal(body) =>
      filters.literal(body.replaceFirst("px$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def expression(xs: Seq[Value]): OGCExpression = xs.head match {
    case Literal(body) => filters.literal(body)
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def concatenatedExpression(xs: Seq[Value]): OGCExpression =
    xs map {
      case Literal(body) => filters.literal(body)
      case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    } reduceLeft { filters.function("strConcat", _, _) }

  def keyword(default: String, xs: Seq[Value]): String = xs.head match {
    case Literal(body) => body
    case _ => default
  }

  def keyword(xs: Seq[Value]): String = keyword(null:String, xs)

  def scale(s: String): Float = {
    if (s.endsWith("%")) s.replaceFirst("%$","").toFloat / 100.0f
    else s.toFloat
  }

  def scale(xs: Seq[Value]): OGCExpression = xs.head match {
    case Literal(l) => filters.literal(scale(l))
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def anchor(xs: Seq[Value]): gt.AnchorPoint = xs map {
    case Literal(l) => filters.literal(scale(l))
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  } take 2 match {
    case Seq(x, y) => styles.createAnchorPoint(x, y)
    case _ => null
  }

  def displacement(xs: Seq[Value]): Seq[OGCExpression] = xs map {
    case Literal(body) =>
      filters.literal(body.replaceFirst("px$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def lengthArray(xs: Seq[Value]): Array[Float] = {
    xs.flatMap(_ match {
      case Literal(body) => Some(body.toFloat)
      case _ => None
    }).toArray
  }

  implicit def stringToFilter(literal: String): org.opengis.filter.Filter = {
    val cql = literal.substring(1, literal.length - 1)
    org.geotools.filter.text.ecql.ECQL.toFilter(cql)
  }

  /**
   * A conversion from Strings to filter Expressions using the ECQL
   * parser.  Strings are assumed to be ECQL expressions if they are wrapped in
   * square braces; otherwise they are taken as literals.
   */
  def stringToExpression(literal: String): OGCExpression =
    if (literal == null) null
    else if ((literal startsWith "[") && (literal endsWith "]")) {
      val cql = literal.substring(1, literal.length - 1)
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    } else {
      filters.literal(literal)
    }

  implicit def valToExpression(v: Value): OGCExpression =
    v match {
      case Expression(cql) =>
        org.geotools.filter.text.ecql.ECQL.toExpression(cql)
      case l: Literal =>
        filters.literal(l.body)
      case _ => null
    }

  def extractFill(props: Map[String, Seq[Value]], markProps: Seq[Property]) = {
    val (externalGraphicUrl, wellKnownMarkName, color) = fill(props("fill"))
    val size = props.get("fill-size") map length
    val rotation = props.get("fill-rotation") map angle
    val opacity = props.get("fill-opacity") map scale getOrElse null

    val graphic = {
      val mark = buildMark(
        wellKnownMarkName,
        size,
        rotation.getOrElse(filters.literal(0)),
        Nil
      )

      val externalGraphic =
        buildExternalGraphic(externalGraphicUrl, props.get("fill-mime").map(keyword))

      if (mark != null || externalGraphic != null) {
        styles.createGraphic(
          externalGraphic,
          mark,
          null,
          null,
          size.getOrElse(null),
          rotation.getOrElse(null)
        )
      } else null
    }

    styles.createFill(color, null, opacity, graphic)
  }

  def extractStroke(props: Map[String, Seq[Value]], markProps: Seq[Property]) = {
    val (externalGraphicUrl, wellKnownMarkName, color) = fill(props("stroke"))
    val dashArray = props.get("stroke-dasharray") map lengthArray getOrElse null
    val dashOffset = props.get("stroke-dashoffset") map length getOrElse null
    val linecap = props.get("stroke-linecap") map expression getOrElse null
    val linejoin = props.get("stroke-linejoin") map expression getOrElse null
    val miterLimit = props.get("stroke-miterlimit") getOrElse null
    val opacity = props.get("stroke-opacity") map scale getOrElse null
    val width = props.get("stroke-width") map length
    val strokeRepeat = props.get("stroke-repeat") map keyword getOrElse "repeat"
    val rotation = props.get("stroke-rotation") map angle getOrElse filters.literal(0)

    val graphic = {
      val mark = buildMark(
        wellKnownMarkName,
        width,
        filters.literal(0),
        Nil
      )

      val externalGraphic =
        buildExternalGraphic(
        externalGraphicUrl,
        props.get("stroke-mime").map(keyword)
      )

      if (mark != null || externalGraphic != null) {
        styles.createGraphic(externalGraphic, mark, null, null, null, rotation)
      } else null
    }

    val graphicStroke = if (strokeRepeat == "repeat") graphic else null
    val graphicFill = if (strokeRepeat == "stipple") graphic else null

    styles.createStroke(
      color,
      width.getOrElse(filters.literal(1)),
      opacity,
      linejoin,
      linecap,
      dashArray,
      dashOffset,
      graphicFill,
      graphicStroke
    )
  }

  /**
   * Convert a set of properties to a set of Symbolizer objects attached to the
   * given Rule.
   */
  def symbolize(rule: Rule): Seq[Pair[Double, Symbolizer]] = {
    val properties = rule.properties

    def orderedMarkRules(symbolizerType: String, order: Int): Seq[Property] =
      rule.context(symbolizerType, order)

    val lineSyms: Seq[(Double, LineSymbolizer)] = 
      (expand(properties, "stroke").toStream zip
       (Stream.from(1) map { orderedMarkRules("stroke", _) })
      ).map { case (props, markProps) =>
        val (_, _, stroke) = fill(props("stroke"))
        val dashArray = props.get("stroke-dasharray") map lengthArray getOrElse null
        val dashOffset = props.get("stroke-dashoffset") map length getOrElse null
        val linecap = props.get("stroke-linecap") map expression getOrElse null
        val linejoin = props.get("stroke-linejoin") map expression getOrElse null
        val miterLimit = props.get("stroke-miterlimit") getOrElse null
        val opacity = props.get("stroke-opacity") map scale getOrElse null
        val width = props.get("stroke-width") map length
        val strokeRepeat = props.get("stroke-repeat") map keyword getOrElse "repeat"
        val rotation = props.get("stroke-rotation") map angle getOrElse filters.literal(0)
        val geom = 
          props.get("stroke-geometry") orElse props.get("geometry") map expression getOrElse null
        val zIndex: Double = 
          props.get("stroke-z-index") orElse props.get("z-index") map {
            x => keyword("0", x).toDouble
          } getOrElse(0d)

        val graphic = buildGraphic("stroke", props, markProps)

        val graphicStroke = 
          for (g <- graphic if strokeRepeat == "repeat") yield g
        val graphicFill =
          for (g <- graphic if strokeRepeat == "stipple") yield g 

        val sym = 
          styles.createLineSymbolizer(
            styles.createStroke(
              stroke,
              width.getOrElse(filters.literal(1)),
              opacity,
              linejoin,
              linecap,
              dashArray,
              dashOffset,
              graphicFill getOrElse(null),
              graphicStroke getOrElse(null)
            ),
            null
          )
        sym.setGeometry(geom)
        (zIndex, sym)
      }

    val polySyms: Seq[(Double, PolygonSymbolizer)] = 
      (expand(properties, "fill").toStream zip
       (Stream.from(1) map { orderedMarkRules("fill", _) })
      ).map { case (props, markProps) =>
        val fillParams = fill(props("fill"))
        val size = props.get("fill-size") map length
        val rotation = props.get("fill-rotation") map angle
        val opacity = props.get("fill-opacity") map scale getOrElse null
        val geom =
          props.get("fill-geometry") orElse props.get("geometry") map expression getOrElse null
        val zIndex: Double = 
          props.get("fill-z-index") orElse props.get("z-index") map {
            x => keyword("0", x).toDouble
          } getOrElse(0d)

        val graphic = buildGraphic("fill", props, markProps) 

        val sym = styles.createPolygonSymbolizer(
          null,
          styles.createFill(
            fillParams._3,
            null,
            opacity,
            graphic getOrElse(null)
          ),
          null
        )
        sym.setGeometry(geom)
        (zIndex, sym)
      }

    val pointSyms: Seq[(Double, PointSymbolizer)] = 
      (expand(properties, "mark").toStream zip
       (Stream.from(1) map { orderedMarkRules("mark", _) })
      ).flatMap { case (props, markProps) => 
        val geom = (props.get("mark-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)
        val zIndex: Double = 
          props.get("mark-z-index") orElse props.get("z-index") map {
            x => keyword(x).toDouble
          } getOrElse(0d)

        val graphic = buildGraphic("mark", props, markProps)

        for (g <- graphic) yield {
          val sym = styles.createPointSymbolizer(g, null)
          sym.setGeometry(geom)
          (zIndex, sym)
        }
      }

    val textSyms: Seq[(Double, TextSymbolizer)] =
      (expand(properties, "label").toStream zip
       (Stream.from(1) map { orderedMarkRules("shield", _) })
      ).map { case (props, shieldProps) => 
        val fillParams = props.get("font-fill").map(fill)
        val fontFamily = props.get("font-family")
        val fontOpacity = props.get("font-opacity").map(scale)
        val anchorPoint = props.get("label-anchor").map(anchor)
        val offset = props.get("label-offset").map(displacement)
        val rotation = props.get("label-rotation").map(angle)
        val geom = (props.get("label-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)
        val zIndex: Double = 
          props.get("label-z-index") orElse props.get("z-index") map {
            x => keyword("0", x).toDouble
          } getOrElse(0d)

        val font = fontFamily.getOrElse(Nil).map { familyName => {
          val fontStyle =
            props.get("font-style").map(expression).getOrElse(filters.literal("normal"))
          val fontWeight =
            props.get("font-weight").map(expression).getOrElse(filters.literal("normal"))
          val fontSize =
            props.get("font-size").map(length).getOrElse(filters.literal("10"))
          styles.createFont(familyName, fontStyle, fontWeight, fontSize)
        }}.toArray

        val fontFill = fillParams.map(fillParams => {
          val mark = buildMark(
            fillParams._2,
            Some(filters.literal(16)),
            filters.literal(0),
            Nil // yeah we're not going to support well-known marks for font fills yet.
          )
          val externalGraphic =
            buildExternalGraphic(fillParams._1, props.get("fill-mime").map(keyword))
          if (mark != null || externalGraphic != null) {
            styles.createGraphic(
              externalGraphic,
              mark,
              null,
              null,
              null,
              null
            )
          } else null
        }).getOrElse(null)

        val haloRadius = props.get("halo-radius").map(length)

        val halo = if (haloRadius.isDefined) {
          val haloColor = props.get("halo-color")
            .map(x => color(x.head)).getOrElse(null)
          val haloOpacity = props.get("halo-opacity").map(scale).getOrElse(null)
          styles.createHalo(
            styles.createFill(haloColor, haloOpacity),
            haloRadius.get
          )
        } else null

        val shield = 
          if (props contains "shield")
            buildGraphic("shield", props, shieldProps)
          else
            None

        val placement = offset match {
          case Some(Seq(d)) => styles.createLinePlacement(d)
          case Some(Seq(x, y)) =>
            styles.createPointPlacement(
              anchorPoint.getOrElse(styles.getDefaultPointPlacement().getAnchorPoint()),
              styles.createDisplacement(x, y),
              rotation.getOrElse(styles.getDefaultPointPlacement().getRotation())
            )
          case _ => null
        }

        val sym = styles.createTextSymbolizer(
          styles.createFill(fillParams.map(_._3).getOrElse(null), null, fontOpacity.getOrElse(null), fontFill),
          font,
          halo,
          concatenatedExpression(props("label")),
          placement,
          null  //the geometry, but only as a string. the setter accepts an expression so we use that instead
        )
        sym.setGeometry(geom)

        // Looks like, depending on GeoTools configuration, this might not be
        // the sort of Symbolizer which supports graphics. Let's at least not
        // cast unless we need to.  
        // TODO: see if there's a nicer way to deal with this that
        shield.foreach { sym.asInstanceOf[TextSymbolizer2].setGraphic(_) }

        for (priority <- props.get("-gt-label-priority") map expression) {
          sym.setPriority(priority)
        }

        for (
          (cssName, sldName) <- gtVendorOpts;
          value <- props.get(cssName)
        ) {
          sym.getOptions().put(
            sldName,
            value.collect({ case Literal(x) => x }).mkString(" ")
          )
        }

        (zIndex, sym)
      }

    Seq(polySyms, lineSyms, pointSyms, textSyms).flatten
  }

  /**
   * Convert a list of tokens (as generated by CssParser.parse) into a GeoTools
   * Style object.  This works by creating individual rules for every possible
   * combination of rules from the input stylesheet, so it takes exponential
   * time (O(2^N) for N= number of input rules).  Be warned.
   *
   * @see org.geotools.styling.SLDTransformer if you want to serialize the
   *   resultant Style object to an XML file
   */
  def css2sld(styleSheet: Seq[Rule]): gt.Style = {
    val sld = styles.createStyle

    val rules = stableSort(styleSheet, Specificity.order _).reverse

    def extractTypeName(rule: Rule): Option[String] =
      flatten(And(rule.selectors)).collect { 
        case Typename(typename) => typename 
      } headOption

    def extractScaleRanges(rule: Rule): Seq[Pair[Option[Double], Option[Double]]] = {
      val scales = 
        flatten(And(rule.selectors))
          .collect { 
            case PseudoSelector("scale", _, d) => d.toDouble
            case Not(PseudoSelector("scale", _, d)) => d.toDouble
          }
          .sorted
          .distinct
      
      val limits = None +: (scales map (Some(_))) :+ None
      limits zip limits.tail
    }

    def isForTypename(typename: Option[String])(rule: Rule): Boolean =
      typename map { t => 
        reduce(allOf(Typename(t) +: rule.selectors)) != Exclude
      } getOrElse true

    def stripTypenames(rule: Rule): Rule =
      rule.copy(selectors = rule.selectors map {
        case Typename(_) => Accept
        case selector => selector
      })

    val typenames = (rules map extractTypeName) distinct

    val styleRules = 
      for (name <- typenames) yield (name, rules filter isForTypename(name) map stripTypenames)

    for ((typename, overlays) <- styleRules) {
      val zGroups: Seq[Seq[(Double, Rule, Seq[gt.Symbolizer])]] = 
        for (rule <- cascading2exclusive(overlays)) yield
          for ((z, syms) <- groupByZ(symbolize(rule))) yield
            (z, rule, syms)

      for ((_, group) <- flattenByZ(zGroups.flatten)) {
        val fts = styles.createFeatureTypeStyle
        typename.foreach { t => fts.featureTypeNames.add(new NameImpl(t)) }
        for ((rule, syms) <- group if !syms.isEmpty) {
          val sldRule = styles.createRule()
          val ranges = extractScaleRanges(rule)

          for (range @ (min, max) <- ranges) {
            val minSelector = min.map(x => PseudoSelector("scale", ">", x.toString))
            val maxSelector = max.map(x => PseudoSelector("scale", "<", x.toString))
            val restricted = 
              reduce(allOf(rule.selectors ++ minSelector ++ maxSelector))

            if (restricted != Exclude) {
              val sldRule = styles.createRule()

              for (m <- min) sldRule.setMinScaleDenominator(m)
              for (m <- max) sldRule.setMaxScaleDenominator(m)
              for (title <- rule.description.title)
                sldRule.getDescription().setTitle(title)
              for (abstrakt <- rule.description.abstrakt)
                sldRule.getDescription().setAbstract(abstrakt)

              val filter = realize(restricted)

              for (f <- filter)
                sldRule.setFilter(f)
              for (sym <- syms)
                sldRule.symbolizers.add(sym)

              fts.rules.add(sldRule)
            }
          }
        }
        sld.featureTypeStyles.add(fts)
      }
    }

    return sld 
  }

  private def flattenByZ[R, S](zGroups: Seq[(Double, R, Seq[S])])
  : Seq[(Double, Seq[(R, Seq[S])])]
  = {
    val zFlattened = zGroups map { case (z, r, s) => (z, (r, s)) }
    (zFlattened groupBy(_._1) mapValues(_ map (_._2)) toSeq).sortBy(_._1)
  }


  private def groupByZ(syms: Seq[(Double, Symbolizer)])
  : Seq[(Double, Seq[Symbolizer])] = {
    // we make a special case for labels; they will be rendered last anyway, so
    // we can fold them into one layer
    val (labels, symbols) = syms partition { _.isInstanceOf[TextSymbolizer] }
    val grouped =
      for {
        (z, syms) <- symbols.groupBy(_._1).toSeq.sortBy(_._1)
      } yield (z, syms map (_._2))
    grouped ++ Seq((0d, labels map (_._2)))
  }

  def simplifyList(sels: Seq[Selector]): Seq[Selector] = {
    if (sels.isEmpty) Seq()
    else {
      val reduced = 
        sels.map(consolidate).reduce {
          (a,b) => reduce[Selector](And(Seq(a, b)))
        }
      reduced match {
        case And(sels) => sels
        case sel               => Seq(sel)
      }
    }
  }

  def consolidate(s: Selector): Selector = {
    def f(s: Selector): Seq[Selector] =
      s match {
        case And(children) => 
          val children0 = children flatMap f
          Seq(
            children0 match {
              case Seq() => Accept
              case Seq(s) => s
              case children0 => And(children0)
            }
          )
        case Or(children) =>
          val children0 = children flatMap f
          Seq(
            children0 match {
              case Seq() => Exclude
              case Seq(s) => s
              case children0 => Or(children0)
            }
          )
        case Not(Not(child)) => Seq(consolidate(child))
        case Not(child) => Seq(Not(consolidate(child)))
        case p => Seq(p)
      }

    f(s).head
  }

  def simplifySelector(r: Rule): Rule =
    r.copy(selectors = simplifyList(r.selectors))

  def merge(a: Rule, b: Rule): Rule = (a merge b)

  def constrain(a: Rule, b: Seq[Selector]): Rule =
    a.copy(selectors = (a.selectors ++ b))

  def cascading2exclusive(xs: Seq[Rule]): Seq[Rule] = {
    import org.geoscript.support.graph._

    val mutuallyExclusive = (a: Rule, b: Rule) =>
      reduce[Selector](And(a.selectors ++ b.selectors)) == Exclude
     
    val cliques = maximalCliques(xs.toSet, mutuallyExclusive)
    val combinations = enumerateCombinations(cliques)

    val ExclusiveRule = EmptyRule.copy(selectors = Seq(Exclude))

    val negate = (x: Rule) =>
      x.copy(selectors = Seq(Not(And(x.selectors))))
    val include = (in: Set[Rule]) =>
      if (in isEmpty) ExclusiveRule else (xs.view filter(in) reduceLeft(merge))
    val exclude = (xs: Seq[Rule]) =>
      xs.map { r => Not(And(r.selectors)) }

    val rulesets = 
      for {
        combo <- combinations
        remainder = xs filterNot(combo contains)
        included = include(combo)
        excluded = exclude(remainder)
        constrained = constrain(included, excluded)
        ruleset = simplifySelector(constrained)
        if ruleset.isSatisfiable
      } yield ruleset

    rulesets.toSeq
  }
}
